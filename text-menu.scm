;;; text-menu.scm -- Provides simple command-line menus and response handlers.
;;;   Copyright © 2012 by Matthew C. Gushee. See LICENSE file for details.


(module text-menu *

        (import scheme)
        (import chicken)
        (import srfi-13)
        (import extras)
        (import data-structures)


;;; ============================================================================
;;; --  SYNTAX DEFINITIONS  ----------------------------------------------------

(define-syntax steps
  (syntax-rules ()
    ((_ (sym proc-or-sym) ...)
       (list (cons sym
                   (if (procedure? proc-or-sym)
                     proc-or-sym
                     (eval proc-or-sym)))
             ...))))

;;; ============================================================================



;;; ============================================================================
;;; --  GLOBAL CONSTANTS & PARAMETERS  -----------------------------------------

(define *custom-loop-choice-function* (make-parameter #f))

(define *all-data* (make-parameter (make-queue)))

(define *current-data* (make-parameter '()))

;;; ============================================================================



;;; ============================================================================
;;; --  UTILITY FUNCTIONS  -----------------------------------------------------

;; Use this to define your own loop choice function. This function
;; should take one parameter, and must return #t to restart the loop
;; or #f to quit the program.
(define (set-loop-choice-function! f)
  (*custom-loop-choice-function* f))

(define (make-prompt-reader message #!optional (default #f))
  (lambda ()
    (let* ((default-string
             (cond
               ((not default) "")
               ((eqv? default 'yes) " [Y/n]")
               ((eqv? default 'no) " [y/N]")
               (else (string-append " [" default "]"))))
           (prompt-string (string-append message default-string ": ")))
      (display prompt-string)
      (let ((input (string-trim-both (read-line))))
        (if (string=? input "")
          (cond
            ((not default) "")
            ((eqv? default 'yes) "y")
            ((eqv? default 'no) "n")
            (else default))
          input)))))

(define (make-validator match-fun #!optional (hint ""))
  (lambda (data)
    (let ((match-result (match-fun data)))
      (if match-result
        (cons #t match-result)
        (begin
          (print "Invalid data! " hint)
          (cons #f data))))))

(define (get-loop-choice)
  (let ((custom-fun (*custom-loop-choice-function*)))
    (if custom-fun
      (custom-fun)
      (let ((input ((make-prompt-reader "Finished one round. Press [RETURN] to start again, or 'q' to quit."))))
        (not (string=? (string-downcase input) "q"))))))

(define (add-to-current-data key value)
  (*current-data* (cons (cons key value) (*current-data*))))

(define (clear-current-data)
  (*current-data* '()))

(define (enqueue-current-data)
  (print "enqueue-current-data")
  (let ((q (*all-data*)))
    (queue-add! q (*current-data*))
    (clear-current-data)))

(define (clear-all-data)
  (*all-data* (make-queue)))

(define (get-all-data)
  (*all-data*))

;;; ============================================================================



;;; ============================================================================
;;; --  GENERIC INTERACTION STEP GENERATOR  ------------------------------------

(define (make-step tag #!key (prompt-msg #f) (default #f) (get-input #f)
                   (validate #f) (required #t) (allow-override #f) (get-error-choice #f)
                   (record #f) (action #f) (choose-next #f))
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
               (make-prompt-reader (string-append error-menu ": "))))
         (record
           (or record
               (lambda (data) (add-to-current-data tag data))))
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
            (let ((data (cdr vres)))
              (record data)
              (action data)
              (choose-next data))
            (let ((error-choice (get-error-choice)))
              (cond 
                ((string=? error-choice "") (loop (get-input)))
                ((string=? error-choice "s") (choose-next 'NONE))
                ((string=? error-choice "a")
                 (let ((data (cdr vres)))
                   (record data)
                   (action data)
                   (choose-next data)))
                ((string=? error-choice "q") 'QUIT)))))))))
      
;;; ============================================================================



;;; ============================================================================
;;; --  MAIN INTERACTION  ------------------------------------------------------

(define (interact steps
                  #!key
                  (on-done (lambda () (enqueue-current-data) (get-all-data)))
                  (on-quit (lambda () (exit)))
                  (looping #f))
  (let* ((step-ids
          (map
            (lambda (elt) (car elt))
            steps))
         (get-next
           (lambda (id)
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
                       (enqueue-current-data)
                       (loop* start))
                     (on-done)))))
              ((LOOP)
               (if (get-loop-choice)
                 (begin
                   (enqueue-current-data)
                   (loop* start))
                 (on-done)))
              ((DONE)
               (on-done))
              ((QUIT)
               (on-quit))
              ((ABORT) (error))
              (else (loop* signal))))
          (error
            (string-append "ERROR: Nonexistent step '"
                           (symbol->string step-id)
                           "' specified.")))))))

;;; ============================================================================

)


;;; Section divider templates

;;; ============================================================================
;;; ----------------------------------------------------------------------------
;;; ============================================================================
