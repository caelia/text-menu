;;; text-menu.scm -- Provides simple command-line menus and response handlers.
;;;   Copyright © 2012 by Matthew C. Gushee. See LICENSE file for details.

(declare
  ;(uses scheme)
  ;(uses chicken)
  (uses srfi-13)
  (uses extras)
  (uses data-structures)
  (uses posix)
  ; (uses srfi-19-date)
  (uses irregex))

(module text-menu *

        (import scheme)
        (import chicken)
        (import srfi-13)
        (import extras)
        (import data-structures)
        (import posix)
        ; (import srfi-19-date)
        (import irregex)


;;; [[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
;;; --  SYNTAX DEFINITIONS  ----------------------------------------------------

(define-syntax make-short-year-converter
  (syntax-rules ()
     ((_ (y0 y1 op addend) ...)
      (lambda (y)
        (cond
          ((and (>= y y0) (<= y y1)) (op y addend))
          ...
          (else y))))
     ((_ false) (lambda (y) y))))

(define-syntax map-step
  (syntax-rules ()
    ((_ (sym proc-or-sym))
     (cons sym
           (if (procedure? proc-or-sym)
             proc-or-sym
             (eval proc-or-sym))))
    ((_ proc)
     (cons (quote proc) proc))))

(define-syntax steps
  (syntax-rules ()
    ((_ step ...)
     (list (map-step step) ...))))

;;; ]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]



;;; [[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
;;; --  GLOBAL CONSTANTS & PARAMETERS  -----------------------------------------

(define +screen-lines+
  (let ((ts (terminal-size (current-output-port))))
    (if (= ts 0)
      20
      (- ts 4))))

(define *text-menu-debug* (make-parameter #f))

(define *custom-loop-choice-function* (make-parameter #f))

(define *all-data* (make-parameter (make-queue)))

(define *current-data* (make-parameter (make-queue)))

;;; ]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]



;;; [[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
;;; --  COMPILED REGULAR EXPRESSIONS  ------------------------------------------

(define yes-rxp (irregex '("Yy")))

(define no-rxp (irregex '("Nn")))

(define blank-rxp (irregex '(* whitespace)))

(define next-rxp (irregex '("Nn")))

(define prev-rxp (irregex '("Pp")))

(define quit-rxp (irregex '("Qq")))

;;; ]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]



;;; [[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
;;; --  UTILITY FUNCTIONS  -----------------------------------------------------


(define (debug-msg . msgs)
  (when (*text-menu-debug*)
    (apply print msgs)))

;; Use this to define your own loop choice function. This function
;; should take one parameter, and must return #t to restart the loop
;; or #f to quit the program.
(define (set-loop-choice-function! f)
  (*custom-loop-choice-function* f))

(define (make-enum #!optional (choices '()) #!key (extensible #f) (store #f)
                   (retrieve #f) (member? #f) (exists? #f) (init (lambda (elts) #f)))
  (let ((store
          (or store
              (lambda (item) (set! choices (append choices (list item))))))
        (retrieve
          (or retrieve
              (lambda () choices)))
        (member?
          (or member?
              (lambda (item) (memq item choices)))))
    (debug-msg "make-enum")
    (when (and exists? (not (exists?)) (not (null? choices)))
      (debug-msg "make-enum: need to initialize")
      (init choices))
    (debug-msg "make-enum: init step done")
    (lambda (cmd . args)
      (case cmd
        ((choices) (retrieve))
        ((extensible?) extensible)
        ((add)
         (let ((new-elt (car args)))
           (cond
             ((not extensible) #f)
             ((member? new-elt) #t)
             (else (store new-elt) #t))))))))

(define (make-prompt-reader message #!optional (default #f))
  (lambda (#!optional new-default)
    (let* ((default (or new-default default))
           (default-string
             (cond
               ((not default) "")
               ((eqv? default 'yes) " [Y/n]")
               ((eqv? default 'no) " [y/N]")
               (else (string-append " [" default "]"))))
           (prompt-string (string-append message default-string ": ")))
      (display prompt-string)
      (let ((input (string-trim-both (read-line))))
        (if (string=? input "")
          (begin (debug-msg "[prompt-reader]: input was blank") (debug-msg "Default: " default)
          (cond
            ((not default) "")
            ((eqv? default 'yes) "y")
            ((eqv? default 'no) "n")
            (else default))
          )
          input)))))

(define (make-validator match-fun #!optional (hint ""))
  (lambda (data)
    (debug-msg "validator")
    (let ((match-result (match-fun data)))
      (if match-result
        (begin
          (debug-msg "validator:#t")
        (cons #t match-result)
        )
        (begin
          (debug-msg "validator:#f")
          (print "Invalid data! " hint)
          (cons #f data))))))

(define (make-regex-validator pattern #!optional (hint ""))
  (let* ((rxp (irregex pattern))
         (match-fun
           (lambda (input) (if (irregex-match rxp input) input #f))))
    (make-validator match-fun hint)))

;; Because I can't seem to use the one from SRFI-19, and this is simple anyway.
(define (leap-year? year)
  (cond
    ((= (modulo year 400) 0) #t) 
    ((= (modulo year 100) 0) #f) 
    ((= (modulo year 4) 0) #t) 
    (else #f)))

(define (get-loop-choice)
  (let ((custom-fun (*custom-loop-choice-function*)))
    (if custom-fun
      (custom-fun)
      (let ((input ((make-prompt-reader "Finished one round. Press [RETURN] to start again, or 'q' to quit."))))
        (not (string=? (string-downcase input) "q"))))))

(define (add-to-current-data key value)
  (queue-add! (*current-data*) (cons key value)))

(define (get-current-data)
  (*current-data*))

(define (clear-current-data)
  (*current-data* (make-queue)))

(define (enqueue-current-data)
  (queue-add! (*all-data*) (*current-data*))
  (clear-current-data))

(define (clear-all-data)
  (*all-data* (make-queue)))

(define (get-all-data)
  (*all-data*))

;;; ]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]



;;; [[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
;;; --  FUNCTIONS USED IN STEPS  -----------------------------------------------


;;; ]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]



;;; [[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
;;; --  GENERIC INTERACTION STEP GENERATOR  ------------------------------------

(define (make-step* tag prompt-msg default get-input validate required
                    allow-override get-error-choice record action choose-next)
  (let* ((prompt-msg
           (or prompt-msg
               (symbol->string tag)))
         (get-input
           (or get-input
               (make-prompt-reader prompt-msg default)))
         (validate
           (or validate
               (make-validator
                 (lambda (s)
                   (debug-msg "[validator]:[default-match-fun]")
                   (if (and (string? s) (string=? s "")) #f s))
                 "Entry cannot be empty.")))
         (error-menu
           (cond
             ((and required (not allow-override))
              "Press RETURN to try again, or 'q' to quit.")
              (required
                "Press RETURN to try again, 'a' to accept the value as is, or 'q' to quit.")
              (allow-override
                "Press RETURN to try again, 'a' to accept the value as is, 's' to skip this item, or 'q' to quit.")
              (else
                "Press RETURN to try again, 's' to skip this item, or 'q' to quit.")))
         (get-error-choice
           (or get-error-choice
               (make-prompt-reader error-menu)))
         (record (or record add-to-current-data))
         (action
           (or action
               (lambda (data) #f)))
         (choose-next
           (or choose-next
               (lambda (data) 'AUTO))))
    (lambda ()
      (let loop ((input (get-input)))
        (let* ((vres (validate input))
               (valid (car vres)))
          (if valid
            (begin (debug-msg "[step:main loop] - input was valid")
            (let ((data (cdr vres)))
              (debug-msg "next - record")
              (record tag data)
              (debug-msg "recorded; next - action")
              (action data)
              (debug-msg "action done; next - choose-next")
              (choose-next data))
            )
            (let ((error-choice (get-error-choice)))
              (cond 
                ((string=? error-choice "") (loop (get-input)))
                ((string=? error-choice "s") (choose-next 'NONE))
                ((string=? error-choice "a")
                 (let ((data (cdr vres)))
                   (record tag data)
                   (action data)
                   (choose-next data)))
                ((string=? error-choice "q") 'QUIT)))))))))
      
;;; ]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]



;;; [[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
;;; --  SIMPLE STEP GENERATOR  -------------------------------------------------

(define (make-step tag #!key (prompt-msg #f) (default #f) (get-input #f)
                          (validate #f) (required #t) (allow-override #f)
                          (get-error-choice #f) (record #f) (action #f)
                          (choose-next #f))
  (make-step* tag prompt-msg default get-input validate required allow-override
              get-error-choice record action choose-next))

;;; ]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]



;;; [[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
;;; --  YESNO-STEP  ------------------------------------------------------------

(define (make-yesno-step tag #!key (prompt-msg #f) (default #f)
                         (required #t) (get-error-choice #f)
                         (record #f) (action #f) (choose-next #f))
  (let ((default* (if default 'yes 'no))
        (validate-yesno
          (make-validator
            (lambda (input)
              (let ((input* (string-downcase input)))
                (cond
                  ((string=? input* "y") (cons #t #t))
                  ((string=? input* "n") (cons #t #f))
                  (else (cons #f #f))))))))
    (make-step* tag prompt-msg default* #f validate-yesno required #f
                get-error-choice record action choose-next)))

;;; ]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]



;;; [[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
;;; --  NUMERIC-STEP  ----------------------------------------------------------

(define (make-numeric-step tag type prompt-msg default required allow-override
                           get-error-choice record action choose-next)
  (let* ((pattern+hint
           (case type
             ((number)
              '((: (? #\-) (or (+ numeric) (: (* numeric) #\. (+ numeric))))
                "Please enter a decimal number. Characters 0-9, ., and - are allowed."))
             ((integer)
              '((: (? #\-) (+ numeric))
                "Please enter an integer. Characters 0-9 and - are allowed."))
             ((posint)
              '((: (/ #\1 #\9) (* numeric))
                "Please enter a positive integer.
                Characters 0-9 are allowed, and the number cannot be 0."))
             ((nonnegint)
              '((+ numeric)
                "Please enter a non-negative integer. Characters 0-9 are allowed."))
             ((float)
              '((: (? #\-) (* numeric) #\. (+ numeric))
                "Please enter a floating-point number."))
             (else
               (error
                 (string-append (symbol->string type)
                                " is not a recognized numeric type.")))))
         (validate (apply make-regex-validator pattern+hint)))
    (make-step* tag prompt-msg default #f validate required allow-override
                get-error-choice record action choose-next)))

;;; ]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]



;;; [[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
;;; --  NUMBER-STEP  -----------------------------------------------------------

(define (make-number-step tag #!key (prompt-msg #f) (default #f)
                          (required #t) (allow-override #f) (get-error-choice #f)
                          (record #f) (action #f) (choose-next #f))
  (make-numeric-step tag 'number prompt-msg default required allow-override
                     get-error-choice record action choose-next))

;;; ]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]



;;; [[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
;;; --  INTEGER-STEP  ----------------------------------------------------------

(define (make-integer-step tag #!key (prompt-msg #f) (default #f)
                           (required #t) (allow-override #f) (get-error-choice #f)
                           (record #f) (action #f) (choose-next #f))
  (make-numeric-step tag 'integer prompt-msg default required allow-override
                     get-error-choice record action choose-next))

;;; ]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]



;;; [[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
;;; --  POSITIVE-INTEGER-STEP  -------------------------------------------------

(define (make-posint-step tag #!key (prompt-msg #f) (default #f)
                          (required #t) (allow-override #f) (get-error-choice #f)
                          (record #f) (action #f) (choose-next #f))
  (make-numeric-step tag 'posint prompt-msg default required allow-override
                     get-error-choice record action choose-next))

;;; ]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]



;;; [[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
;;; --  NON-NEGATIVE-INTEGER-STEP  ---------------------------------------------

(define (make-nonnegint-step tag #!key (prompt-msg #f) (default #f)
                             (required #t) (allow-override #f) (get-error-choice #f)
                             (record #f) (action #f) (choose-next #f))
  (make-numeric-step tag 'nonnegint prompt-msg default required allow-override
                     get-error-choice record action choose-next))

;;; ]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]



;;; [[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
;;; --  FLOAT-STEP  ------------------------------------------------------------

(define (make-float-step tag #!key (prompt-msg #f) (default #f)
                         (required #t) (allow-override #f) (get-error-choice #f)
                         (record #f) (action #f) (choose-next #f))
  (make-numeric-step tag 'float prompt-msg default required allow-override
                     get-error-choice record action choose-next))

;;; ]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]



;;; [[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
;;; --  DATE-STEP  -------------------------------------------------------------

(define (make-date-step tag #!key (prompt-msg #f) (required #t) (allow-override #f)
                        (short-year-rules '((60 99 + 1900) (0 59 + 2000))) (allowed-years '(1 3000))
                        (default-year-rule 'last-or-current) (default-month-rule 'last-or-current)
                        (get-error-choice #f) (record #f) (action #f) (choose-next #f))
  (let* ((date-rxp
           (irregex
             '(: (or
                   (: (? (: (? (: (=> yr (** 1 4 numeric)) #\-)) (=> mo (** 1 2 numeric)) #\-)) (=> da (** 1 2 numeric)))
                   (: (? (: (=> mo (** 1 2 numeric)) #\/)) (=> da (** 1 2 numeric)))
                   (: (=> mo (** 1 2 numeric)) #\/ (=> da (** 1 2 numeric)) #\/ (=> yr (** 1 4 numeric)))))))
         (string->ymd
           (lambda (s)
             (debug-msg "string->ymd")
             (if (string=? s "")
               (begin (debug-msg "blank input")
               '(#f #f #f)
               )
               (let ((match (irregex-match date-rxp s)))
                 (debug-msg "match result: " match)
                 (if match
                    (begin (debug-msg "matched")
                    (let ((yr (irregex-match-substring match 'yr))
                          (mo (irregex-match-substring match 'mo))
                          (da (irregex-match-substring match 'da)))
                      (list (and yr (string->number yr))
                            (and mo (string->number mo))
                            (and da (string->number da))))
                    )
                    '(#f #f #f))))))
         (last-entered-year #f)
         (last-entered-month #f)
         (current-year
           (lambda ()
             (let ((t (seconds->local-time (current-seconds))))
               (+ (vector-ref t 5) 1900))))
         (current-month
           (lambda ()
             (let ((t (seconds->local-time (current-seconds))))
               (+ (vector-ref t 4) 1))))
         (get-default-year
           (lambda ()
             (debug-msg "get-default-year")
             (case default-year-rule
               ((last)
                (debug-msg "last - " last-entered-year)
                last-entered-year)
               ((current)
                (debug-msg "current")
                (current-year))
               ((last-or-current)
                (debug-msg "last-or-current - " last-entered-year)
                (or last-entered-year (current-year)))
               ((none)
                (debug-msg "none")
                #f))))
         (get-default-month
           (lambda ()
             (case default-month-rule
               ((last) last-entered-month)
               ((current) (current-month))
               ((last-or-current) (or last-entered-month (current-month)))
               ((none) #f))))
         (short-conv
           (make-short-year-converter short-year-rules))
         (canonicalize-date
           (lambda (ymd)
             (debug-msg "canonicalize-date")
             (let ((y (car ymd))
                   (m (cadr ymd))
                   (d (caddr ymd)))
               (when (and y (not m))
                 (error "Invalid date."))
               (debug-msg "don't have year without month")
               (let ((y* (or y (get-default-year)))
                     (m* (or m (get-default-month))))
                 (when (or (not y*) (not m*))
                   (error "Invalid date."))
                 (let ((y** (if (>= y* 100)
                           y*
                           (short-conv y*))))
                   (debug-msg (string-append "Year: " (number->string y**)))
                   (set! last-entered-year y**)
                   (set! last-entered-month m*)
                   (list y** m* d))))))
         (date-validator
           (lambda (ymd)
             (let ((y (car ymd))
                   (m (cadr ymd))
                   (d (caddr ymd))
                   (first
                     (or (and (number? allowed-years) allowed-years)
                         (car allowed-years)))
                   (last
                     (or (and (number? allowed-years) allowed-years)
                         (cadr allowed-years)))
                   (invalid-result
                     (lambda ()
                       (print "Invalid input!")
                       (cons #f ymd))))
               (cond
                 ((not d) (invalid-result))
                 ((< y first) (invalid-result))
                 ((> y last) (invalid-result))
                 ((< m 1) (invalid-result))
                 ((> m 12) (invalid-result))
                 ((< d 1) (invalid-result))
                 ((and (memv m '(1 3 5 7 8 10 12)) (> d 31)) (invalid-result))
                 ((and (memv m '(4 6 9 11)) (> d 30)) (invalid-result))
                 ((and (= m 2) (not (leap-year? y)) (> d 28)) (invalid-result))
                 ((and (= m 2) (> d 29)) (invalid-result))
                 (else (cons #t ymd))))))
         (get-default-string
           (lambda ()
             (let ((defyear (get-default-year))
                   (defmonth (get-default-month)))
               (cond
                 ((and defyear defmonth)
                  (sprintf "~A-~A" defyear defmonth))
                 (defyear
                   (number->string defyear))
                 (else #f)))))
         (prompt-reader
           (make-prompt-reader (or prompt-msg (symbol->string tag))))
         (input-getter
           (lambda ()
             (canonicalize-date (string->ymd (prompt-reader (get-default-string)))))))
    (make-step* tag prompt-msg #f input-getter date-validator required
                allow-override get-error-choice record action choose-next)))

;;; ]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]



;;; [[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
;;; --  ENUM-STEP  -------------------------------------------------------------

(define (make-choice-menu head-msg prompt-msg choices)
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

(define (make-enum-step tag enum #!key (head-msg #f) (prompt-msg #f)
                        (required #t) (get-error-choice #f) (record #f)
                        (action #f) (choose-next #f) (extend-key "+"))
  (let* ((choices (enum 'choices))
         (len (length choices))
         (extensible (enum 'extensible?))
         (head-msg (or head-msg "Please choose from the following:"))
         (extend-portion
           (if extensible
             (string-append " or '" extend-key "' to add a new item")
             ""))
         (prompt-msg
           (or prompt-msg
               (string-append "Enter the number of your choice"
                              extend-portion)))
         (menu (make-choice-menu head-msg prompt-msg choices))
         (get-input (lambda () (string-trim-both (read-line))))
         (posint-rxp (irregex '(: (/ #\1 #\9) (* numeric))))
         (get-choice
           (lambda ()
             (let loop ((signal 'start))
               (menu signal)
               (let ((input (get-input)))
                 (cond
                   ((and (enum 'extensible?) (string=? input extend-key))
                    (display "Enter new item: ")
                    (let ((input* (get-input)))
                      (enum 'add input*)
                      input*))
                   ((irregex-match next-rxp input)
                    (loop 'next))
                   ((irregex-match prev-rxp input)
                    (loop 'previous))
                   ((irregex-match posint-rxp input)
                    (let ((idx (- (string->number input) 1)))
                      (and (< idx len)
                           (list-ref choices idx))))
                   (else #f))))))
         (validate
           (lambda (input)
             (if input
               (cons #t input)
               (begin
                 (print "Invalid input!")
                 (cons #f ""))))))
    (make-step* tag #f #f get-choice validate required #f get-error-choice
                record action choose-next)))

;;; ]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]



;;; [[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
;;; --  MAIN INTERACTION  ------------------------------------------------------

(define (interact steps
                  #!key
                  (on-done (lambda () (enqueue-current-data)))
                  (on-quit (lambda () (exit)))
                  (before-iteration (lambda () #f))
                  (after-iteration (lambda () #f))
                  (looping #f))
  (let* ((step-ids
          (map
            (lambda (elt) (car elt))
            steps))
         (get-next
           (lambda (id)
             (debug-msg "interact:get-next")
             (let loop ((ids* step-ids)
                        (found #f))
               (cond
                 ((null? ids*) #f)
                 (found (car ids*))
                 ((eqv? id (car ids*)) (loop (cdr ids*) #t))
                 (else (loop (cdr ids*) #f))))))
         (start (car step-ids)))
    (clear-all-data)
    (let loop* ((step-id start))
      (let ((step (alist-ref step-id steps)))
        (if step
          (let* ((signal (step))) 
            (case signal
              ((AUTO)
               (let ((next (get-next step-id)))
                 (if next
                   (loop* next)
                   (if (and looping (get-loop-choice))
                     (begin
                       (after-iteration)
                       (enqueue-current-data)
                       (loop* start))
                     (begin
                       (after-iteration)
                       (on-done))))))
              ((LOOP)
               (if (get-loop-choice)
                 (begin
                   (after-iteration)
                   (enqueue-current-data)
                   (loop* start))
                 (on-done)))
              ((DONE)
               (after-iteration)
               (on-done))
              ((QUIT)
               (on-quit))
              ((ABORT) (error))
              (else (loop* signal))))
          (error
            (string-append "ERROR: Nonexistent step '"
                           (symbol->string step-id)
                           "' specified.")))))))

;;; ]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]

)


;;; Section divider templates

;;; [[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
;;; ----------------------------------------------------------------------------
;;; ]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]



