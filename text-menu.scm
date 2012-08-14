;;; text-menu.scm -- Provides simple command-line menus and response handlers.
;;;   Copyright © 2012 by Matthew C. Gushee. See LICENSE file for details.

(use input-parse)
(use srfi-69)
(use irregex)


;;; ============================================================================
;;; --  GLOBAL DATA STRUCTURES  ------------------------------------------------

(define *recorder*
  (make-parameter

;; A stopgap until we figure out how to detect terminal height
(define +screen-lines+ 20)

(define *enums* (make-parameter (make-hash-table)))

(define *steps* (make-parameter (make-hash-table)))

;;; ============================================================================



;;; ============================================================================
;;; --  RESPONSE VALIDATORS  ---------------------------------------------------

(define (string-validator s) #t)

(define yesno-rxp (irregex '(: (* whitespace) ("[NnYy]") (* whitespace))))
(define (yesno-validator yn)
  (lambda (resp)
    (irregex-match yesno-rxp resp)))

(define (number-validator n)
  #f)

(define (enum-validator enum-name)
  (let ((choices (hash-table-ref (*enums*) enum-name)))
    (lambda (resp)
      (memq resp choices))))

;;; ============================================================================



;;; ============================================================================
;;; --  ENVIRONMENT SETUP  -----------------------------------------------------

(define (set-recorder #!optional (recorder 'default))
  (if (eqv? recorder 'default)
    (*recorder*
      (let ((data (make-hash-table)))
        (lambda (arg . args)
          (cond
            ((eqv? arg 'get) data)
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
               ((mem?)
                (memq (car args) elts*))))))
    (hash-table-set! (*enums*) enum-name enum)))

(define (set-step! step-id #!key
                   (required #t) (type 'string) (validator #f)
                   (action 'record) (next #f) (branch #f))
  (let* ((validator*
           (cond
             ((procedure? validator) validator)
             ((eqv? type 'string) string-validator)
             ((eqv? type 'yesno) yesno-validator)
             ((eqv? type 'number) number-validator)
             ((eqv? type 'integer) integer-validator)
             ((eqv? type 'float) float-validator)
             ((and (list? type) (eqv? (car type) 'enum))
              (enum-validator (cadr type)))))
         (action*
           (if (eqv? action 'record)
             '()
             action))

  (let ((step-function
          (lambda 
  (hash-table-set! (*steps*) step-id
                   (lambda ()
                     (let 

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
;;; ----------------------------------------------------------------------------
;;; ============================================================================
