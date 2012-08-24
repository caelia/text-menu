;;; text-menu.scm -- Provides simple command-line menus and response handlers.
;;;   Copyright © 2012 by Matthew C. Gushee. See LICENSE file for details.

(use input-parse)
; (use srfi-69)
(use redis-client)   ; Don't really want this dependency, but need persistence.
(use redis-extras)   ; See above
(use irregex)
(use posix)
(use srfi-19)
(use s11n)


(module text-menu *
;        ( set-recorder!
;          register-enum
;          set-step!
;          run )

        (import scheme)
        (import chicken)
        (import extras)
        (import ports)
        (import srfi-13)
        (import input-parse)
        (import redis-client)
        (import redis-extras)
        (import irregex)
        (import posix)
        (import srfi-19) 
        (import s11n)


;;; ============================================================================
;;; --  SYNTAX DEFINITIONS  ----------------------------------------------------

(define-syntax short-year-conversions
  (syntax-rules ()
     ((_ (y0 y1 op addend) ...)
      (lambda (y)
        (cond
          ((and (>= y y0) (<= y y1)) (op y addend))
          ...
          (else y))))))

;;; ============================================================================



;;; ============================================================================
;;; --  GLOBAL DATA STRUCTURES & CONSTANTS  ------------------------------------

(define *app-name* (make-parameter #f))

(define *start-step* (make-parameter #f))

(define *recorder* (make-parameter #f))

(define +screen-lines+
  (let ((ts (terminal-size (current-output-port))))
    (if (= ts 0)
      20
      (- ts 4))))

;; A hash-table-like object that will eventually allow interchangeable storage
;;   mechanisms. The current implementation depends on Redis.
; (define (make-hash-proxy proxy-tag)
;   (lambda (cmd . args)
;     (case cmd
;       ((get) (redis-hget proxy-tag (car args)))
;       ((set!) (redis-hset proxy-tag (car args) (cadr args)))
;       ((set-tag!) (set! proxy-tag (car args))))))

; (define (hash-proxy-ref proxy key)
;   (proxy 'get key))
; 
; (define (hash-proxy-set! proxy key value)
;   (proxy 'set! key value))

(define *enums* (make-parameter (make-hash-proxy "@enums")))

(define *steps* (make-parameter (make-hash-proxy "@steps")))

(define *error-message-pause* (make-parameter 3))     ; value in seconds

(define *allowed-years* (make-parameter '(1 3000)))

(define *last-entered-year* (make-parameter #f))

(define *last-entered-month* (make-parameter #f))

(define *default-year-rule* (make-parameter 'last-or-current))    ; Could also be 'last, 'current, or 'none

(define *default-month-rule* (make-parameter 'last-or-current))

(define *short-year-converter* (make-parameter (short-year-conversions (60 99 + 1900) (0 59 + 2000))))

;;; ============================================================================



;;; ============================================================================
;;; --  COMPILED REGULAR EXPRESSIONS  ------------------------------------------

(define yes-rxp (irregex '("Yy")))

(define no-rxp (irregex '("Nn")))

(define blank-rxp (irregex '(* whitespace)))

(define next-rxp (irregex '("Nn")))

(define prev-rxp (irregex '("Pp")))

(define quit-rxp (irregex '("Qq")))

(define number-rxp (irregex '(: (? #\-) (or (+ numeric) (* numeric) #\. (+ numeric)))))

(define float-rxp (irregex '(: (? #\-) (* numeric) #\. (+ numeric))))

(define integer-rxp (irregex '(: (? #\-) (+ numeric))))

(define nonnegint-rxp (irregex '(+ numeric)))

(define posint-rxp (irregex '(: (/ #\1 #\9) (* numeric))))

(define date-rxp
  (irregex '(: (or
                 (: (? (: (? (: (=> yr (** 1 4 numeric)) #\-)) (=> mo (** 1 2 numeric)) #\-)) (=> da (** 1 2 numeric)))
                 (: (? (: (=> mo (** 1 2 numeric)) #\/)) (=> da (** 1 2 numeric)))
                 (: (=> mo (** 1 2 numeric)) #\/ (=> da (** 1 2 numeric)) #\/ (=> yr (** 1 4 numeric)))
                 ))))

;;; ============================================================================



;;; ============================================================================
;;; --  UTILITY FUNCTIONS  -----------------------------------------------------

;; Serialize any object to a string
(define (obj->string o)
  (with-output-to-string
    (lambda ()
      (serialize o))))

;; Deserialize any object from a string
(define (string->obj s)
  (with-input-from-string s
    (lambda ()
      (deserialize))))

;; I think eventually we will use srfi-19 for this, but let's keep it simple for now.
(define (ymd->string ymd)
  (let ((y (car ymd))
        (m (cadr ymd))
        (d (caddr ymd)))
    (string-append
      (number->string y) "-"
      (number->string m) "-"
      (number->string d))))


(define (string->ymd s)
  (let ((match (irregex-match date-rxp s)))
    (and match
         (list (string->number (irregex-match-substring match 'yr))
               (string->number (irregex-match-substring match 'mo))
               (string->number (irregex-match-substring match 'da))))))

(define (current-year)
  (let ((t (seconds->local-time (current-seconds))))
    (+ (vector-ref t 5) 1900)))

(define (current-month)
  (let ((t (seconds->local-time (current-seconds))))
    (+ (vector-ref t 4) 1)))

(define (get-default-year)
  (case (*default-year-rule*)
    ((last) (*last-entered-year*))
    ((current) (current-year))
    ((last-or-current) (or (*last-entered-year*) (current-year)))
    ((none) #f)))

(define (get-default-month)
  (case (*default-month-rule*)
    ((last) (*last-entered-month*))
    ((current) (current-month))
    ((last-or-current) (or (*last-entered-month*) (current-month)))
    ((none) #f)))


(define (canonicalize-date ymd)
  (let ((y (car ymd))
        (m (cadr ymd))
        (d (caddr ymd)))
    (when (and y (not m))
      (error "Invalid date."))
    (let ((y* (or y (get-default-year)))
          (m* (or m (get-default-month))))
      (when (or (not y*) (not m*))
        (error "Invalid date."))
      (let ((y**
              (if (>= y* 100)
                y*
                (let ((conv (*short-year-converter*)))
                  (if conv (conv y*) y*)))))
        (*last-entered-year* y**)
        (*last-entered-month* m*)
        (list y** m* d)))))


;;; ============================================================================



;;; ============================================================================
;;; --  RESPONSE VALIDATORS  ---------------------------------------------------

(define (blank? s)
  (irregex-match blank-rxp s))

(define (string-validator s) s)

(define (yesno-validator yn)
  (cond
    ((irregex-match yes-rxp yn) 'yes)
    ((irregex-match no-rxp yn) 'no)
    (else #f)))

(define (number-validator n)
  (and (irregex-match number-rxp n) n))

(define (float-validator n)
  (and (irregex-match float-rxp n) n))

(define (integer-validator n)
  (and (irregex-match integer-rxp n) n))

(define (nonnegint-validator n)
  (and (irregex-match nonnegint-rxp n) n))

(define (posint-validator n)
  (and (irregex-match posint-rxp n) n))

(define (enum-validator enum)
  (let* ((choices (enum 'choices))
         (extensible (enum 'extensible?))
         (len (length choices)))
    (lambda (resp)
      (let ((idx (string->number resp)))
        (if (>= idx len)
          #f
          (list-ref choices idx))))))

(define (date-validator ymd)
  (let* ((y (car ymd))
         (m (cadr ymd))
         (d (caddr ymd))
         (allowed (*allowed-years*))
         (first
           (or (and (number? allowed) allowed)
               (car allowed)))
         (last
           (or (and (number? allowed) allowed)
               (cadr allowed))))
    (cond
      ((< y first) #f)
      ((> y last) #f)
      ((< m 1) #f)
      ((> m 12) #f)
      ((< d 1) #f)
      ((and (memv m '(1 3 5 7 8 10 12)) (> d 31)) #f)
      ((and (memv m '(4 6 9 11)) (> d 30)) #f)
      ((and (= m 2) (not (leap-year? y)) (> d 28)) #f)
      ((and (= m 2) (> d 29)) #f)
      (else ymd))))


;;; ============================================================================



;;; ============================================================================
;;; --  PROMPT FUNCTIONS  ------------------------------------------------------

(define (prompt-for-string msg . default)
  (if (null? default)
    (display (string-append msg ": "))
    (display (string-append msg " [" (car default) "]: "))))

(define (prompt-for-yesno msg . default)
  (if (null? default)
    (prompt-for-string msg)
    (let ((defstring
            (if (car default) "Y/n" "y/N")))
      (prompt-for-string msg defstring))))

(define (prompt-for-date msg)
  (let* ((year (get-default-year))
         (month (if year (get-default-year) #f))
         (default-string
           (if month
             (sprintf "~A-~A" year month)
             (if year
               (number->string year)
               #f))))
    (if default-string
      (prompt-for-string msg default-string)
      (prompt-for-string msg))))


;; Making this a closure allows for paging
(define (choice-menu head-msg prompt-msg choices #!optional (extensible #f))
  (let* ((page 0)
         (len (length choices))
         (pages (quotient len +screen-lines+))
         (show-page
           (lambda ()
             (if (> page pages)
               #f
               (let* ((start (* page +screen-lines+))
                      (end (min (+ start +screen-lines+) len)))
                 (let loop ((i start))
                   (if (>= i end)
                     (begin
                       (newline)
                       (display (string-append prompt-msg ": ")))
                     (begin
                       (let ((new-i (+ 1 i)))
                         (print new-i ") " (list-ref choices i))
                         (loop new-i))))))))))
    (lambda (cmd)
      (case cmd
        ((start)
         (print head-msg)
         (newline)
         (show-page))
        ((next)
         (when (< page pages)
           (set! page (+ page 1))
           (show-page)))
        ((previous)
         (when (> page 0)
           (set! page (- page 1))
           (show-page)))))))


(define (loop-prompt)
  (display "Press RETURN to repeat, or 'q' to quit: "))

;;; ============================================================================



;;; ============================================================================
;;; --  ENVIRONMENT SETUP  -----------------------------------------------------

(define (set-recorder! #!optional (recorder 'default) #!key (tag "@data"))
  (if (eqv? recorder 'default)
    (*recorder*
      (let ((data (make-hash-proxy tag))
            (step-tag #f))
        (lambda (arg . args)
          (cond
            ((eqv? arg 'init) (set! step-tag (car args)))
            ((eqv? arg 'get) (list step-tag data))
            ((null? args) (hash-proxy-ref data arg))
            (else (hash-proxy-set! data arg (car args)))))))
    (*recorder*
      recorder)))

(define (register-enum enum-name #!key (extensible #f) (elts '()))
  (let* ((enum
           (lambda (cmd . args)
             (case cmd
               ((extensible?)
                extensible)
               ((add)
                (if extensible
                  (set! elts (append elts args))
                  (abort "This enum is not extensible.")))
               ((choices)
                elts)
               ((mem?)
                (memq (car args) elts))
               ((length)
                (length elts))))))
    (hash-proxy-set! (*enums*) enum-name (obj->string enum))))

(define (set-step! step-tag #!key
                   (menu-msg "Please choose from the following options:")
                   (prompt-msg #f) (default '()) (valid-input-hint #f)
                   (required #t) (type 'string) (validator #f)
                   (record #t) (action #f) (next 'END) (branch (lambda (resp) #f)))
  (let* ((enum
           (if (and (list? type) (eqv? (car type) 'enum))
             (string->obj (hash-proxy-ref (*enums*) (cadr type)))
             #f))
         (choices (and enum (enum 'choices)))
         (menu
           (if choices
             (let ((choices*
                     (if (enum 'extensible?)
                       (append choices (list "Add New"))
                       choices))
                   (prompt-msg* (or prompt-msg "Enter the number of your choice: ")))
               (choice-menu menu-msg prompt-msg choices*))
             #f))
         (validate
           (cond
             ((procedure? validator) validator)
             ((eqv? type 'string) string-validator)
             ((eqv? type 'yesno) yesno-validator)
             ((eqv? type 'number) number-validator)
             ((eqv? type 'integer) integer-validator)
             ((eqv? type 'float) float-validator)
             ((eqv? type 'date) date-validator)
             (enum (enum-validator enum))))
         (record*
           (cond
             ((procedure? record) record)
             (record (*recorder*))
             (else (lambda (k v) #f))))
         (action*
           (or action (lambda (k v) #f)))
         (get-input
           (cond
             (menu
               (lambda ()
                 (let loop ((cmd 'start))
                   (menu cmd)
                   (let ((input (string-trim-both (read-text-line))))
                     (cond
                       ((irregex-match next-rxp input) (loop 'next))
                       ((irregex-match prev-rxp input) (loop 'previous))
                       (else input))))))
             ((eqv? type 'date)
              (lambda ()
                (display prompt-msg)
                (let* ((input (string-trim-both (read-text-line)))
                       (ymd (string->ymd input)))
                  (canonicalize-date ymd))))
             (else
               (lambda ()
                 (display prompt-msg)
                 (string-trim-both (read-text-line))))))
         (on-error
           (lambda ()
             (print "Invalid input!")
             (when valid-input-hint
               (print valid-input-hint))
             (sleep (*error-message-pause*))
             (system "clear")))
         (process
           (lambda (input)
             (record* step-tag input)
             (action* step-tag input)
             (or (branch input) next)))
         (step-fun
           (lambda ()
             (let loop ((input (get-input)))
               (if (validate input)
                 (process input)
                 (begin
                   (on-error)
                   (loop (get-input))))))))
    (hash-proxy-set! (*steps*) step-tag (obj->string step-fun))))

(define (set-start! step-tag)
  (*start-step* step-tag))

(define (set-appname name)
  (*app-name* name))


;;; ============================================================================



;;; ============================================================================
;;; --  MAIN INTERACTION  ------------------------------------------------------

(define (run appname step-tag)
  (*app-name* appname)
  (*start-step* step-tag)
  (redex-init appname)
  (let loop ((step (string->obj (hash-proxy-ref (*steps*) step-tag))))
    (let ((result (step)))
      (cond
        ((eqv? result 'END)
         #t)
        ((eqv? result 'LOOP)
         (let loop* ()
           (loop-prompt)
           (let ((choice (string-trim-both (read-text-line))))
             (cond
               ((irregex-match quit-rxp choice) #t)
               ((string=? choice "") (loop (hash-proxy-ref (*steps*) (*start-step*))))
               (else (loop*))))))
        (else
          (loop (string->obj (hash-proxy-ref (*steps*) result))))))))

;;; ============================================================================

)


;;; ============================================================================
;;; ----------------------------------------------------------------------------
;;; ============================================================================

