;;; text-menu.scm -- Provides simple command-line menus and response handlers.
;;;   Copyright © 2012 by Matthew C. Gushee. See LICENSE file for details.

(use input-parse)
; (use srfi-69)
(use redis-client)   ; Don't really want this dependency, but need persistence.
(use redis-extras)   ; See above
(use irregex)
(use posix)
(use simple-sha1)


(module text-menu
        ( set-recorder!
          register-enum
          set-step!
          run )


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
(define (make-hash-proxy proxy-tag)
  (lambda (cmd . args)
    (case cmd
      ((get) (redis-hget proxy-tag (car args)))
      ((set!) (redis-hset proxy-tag (car args) (cadr args)))
      ((set-tag!) (set! proxy-tag (car args))))))

(define (hash-proxy-ref proxy key)
  (proxy 'get key))

(define (hash-proxy-set! proxy key value)
  (proxy 'set! key value))

(define *enums* (make-parameter (make-hash-table)))

(define *steps* (make-parameter (make-hash-table)))

(define *error-message-pause* (make-parameter 3))     ; value in seconds

;;; ============================================================================



;;; ============================================================================
;;; --  COMPILED REGULAR EXPRESSIONS  ------------------------------------------

(define yesno-rxp (irregex '("[NnYy]")))

(define blank-rxp (irregex '(* whitespace)))

(define next-rxp (irregex '("[Nn]")))

(define prev-rxp (irregex '("[Pp]")))

(define number-rxp (irregex '(: (? #\-) (or (+ numeric) (* numeric) #\. (+ numeric)))))

(define float-rxp (irregex '(: (? #\-) (* numeric) #\. (+ numeric))))

(define integer-rxp (irregex '(: (? #\-) (+ numeric))))

(define nonnegint-rxp (irregex '(+ numeric)))

(define posint-rxp (irregex '(: (/ #\1 #\9) (* numeric))))

;;; ============================================================================



;;; ============================================================================
;;; --  RESPONSE VALIDATORS  ---------------------------------------------------

(define (blank? s)
  (irregex-match blank-rxp s))

(define (string-validator s) #t)

(define (yesno-validator yn)
  (lambda (resp)
    (irregex-match yesno-rxp resp)))

(define (number-validator n)
  (irregex-match number-rxp n))

(define (float-validator n)
  (irregex-match float-rxp n))

(define (integer-validator n)
  (irregex-match integer-rxp n))

(define (nonnegint-validator n)
  (irregex-match nonnegint-rxp n))

(define (posint-validator n)
  (irregex-match posint-rxp n))

(define (enum-validator enum-name)
  (let ((choices (hash-table-ref (*enums*) enum-name)))
    (lambda (resp)
      (memq resp choices))))

;;; ============================================================================



;;; ============================================================================
;;; --  ENVIRONMENT SETUP  -----------------------------------------------------

(define (set-recorder! #!optional (recorder 'default))
  (if (eqv? recorder 'default)
    (*recorder*
      (let ((data (make-hash-table))
            (step-tag #f))
        (lambda (arg . args)
          (cond
            ((eqv? arg 'init) (set! step-tag (car args)))
            ((eqv? arg 'get) (list step-tag data))
            ((null? args) (hash-table-ref data arg))
            (else (hash-table-set! data arg (car args)))))))
    (*recorder*
      recorder)))

(define (register-enum enum-name #!key (extensible #f) (elts '()))
  (let* ((elts* elts)
         (enum
           (lambda (cmd . args)
             (case cmd
               ((extensible?)
                extensible)
               ((add)
                (if extensible
                  (set! elts* (append elts* args))
                  (abort "This enum is not extensible.")))
               ((get)
                elts*)
               ((mem?)
                (memq (car args) elts*))
               ((length)
                (length *elts))))))
    (hash-table-set! (*enums*) enum-name enum)))

(define (set-step! step-tag #!key
                   (menu-msg "Please choose from the following options:")
                   (prompt-msg #f) (default '()) (valid-input-hint #f)
                   (required #t) (type 'string) (validator #f)
                   (record #t) (action #f) (next 'END) (branch (lambda (resp) #f)))
  (let* ((menu
           (if (and (list? type) (eqv? (car type) 'enum))
             (let ((choices ((hash-table-ref (*enums*) (cadr type)) 'get))
                   (prompt-msg* (or prompt-msg "Enter the number of your choice: ")))
               (choice-menu menu-msg prompt-msg choices))
             #f))
         (validate
           (cond
             ((procedure? validator) validator)
             ((eqv? type 'string) string-validator)
             ((eqv? type 'yesno) yesno-validator)
             ((eqv? type 'number) number-validator)
             ((eqv? type 'integer) integer-validator)
             ((eqv? type 'float) float-validator)
             ((and (list? type) (eqv? (car type) 'enum))
              (enum-validator (cadr type)))))
         (record*
           (cond
             ((procedure? record) record)
             (record (*recorder*))
             (else (lambda (k v) #f))))
         (action*
           (or action (lambda (k v) #f)))
         (get-input
           (if menu
             (lambda ()
               (let loop ((cmd 'start))
                 (menu cmd)
                 (let ((input (string-trim-both (read-text-line))))
                   (cond
                     ((irregex-match next-rxp input) (loop 'next))
                     ((irregex-match prev-rxp input) (loop 'previous))
                     (else input)))))
             (lambda ()
               (display prompt-msg)
               (string-trim-both (read-text-line)))))
         (on-error
           (lambda ()
             (print "Invalid input!")
             (when valid-input-hint
               (print valid-input-hint))
             (sleep (*error-message-pause*))
             (system "clear")))
         (process
           (lambda (input)
             (record step-tag input)
             (action step-tag input)
             (or (branch input) next)))
         (step-fun
           (lambda ()
             (let loop ((input (get-input)))
               (if (validate input)
                 (process input)
                 (begin
                   (on-error)
                   (loop (get-input))))))))
    (hash-table-set! (*steps*) step-tag step-fun)))

(define (set-start! step-tag)
  (*start-step* step-tag))

(define (set-appname name)
  (*app-name* name))


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

;; Making this a closure allows for paging
(define (choice-menu head-msg prompt-msg choices)
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
;;; --  MAIN INTERACTION  ------------------------------------------------------

(define (run appname step-tag)
  (*app-name* appname)
  (*start-step* step-tag)
  (redex-init appname)
  (let loop ((step (hash-table-ref (*steps*) step-tag)))
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
               ((string=? choice "") (loop (hash-table-ref (*steps*) (*start-step*))))
               (else (loop*))))))
        (else
          (loop (hash-table-ref (*steps*) result)))))))

;;; ============================================================================





;;; ============================================================================
;;; ----------------------------------------------------------------------------
;;; ============================================================================
