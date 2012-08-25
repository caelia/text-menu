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

;;; ============================================================================



;;; ============================================================================
;;; --  UTILITY FUNCTIONS  -----------------------------------------------------

;; Use this to define your own loop choice function. This function
;; should take one parameter, and must return #t to restart the loop
;; or #f to quit the program.
(define (set-loop-choice-function! f)
  (*custom-loop-choice-function* f))

(define (default-get-input message #!optional (default #f))
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

(define (get-loop-choice data)
  (let ((custom-fun (*custom-loop-choice-function*)))
    (if custom-fun
      (custom-fun data)
      (let ((input ((default-get-input "Finished one round. Press [RETURN] to start again, or 'q' to quit."))))
        (not (string=? (string-downcase input) "q"))))))

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
               (default-get-input prompt-msg default)))
         (validate
           (or validate
               (lambda (s)
                 (if (and (string? s)
                          (string=? s ""))
                   (cons #f s)
                   (cons #t s)))))
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
               (default-get-input (string-append "Invalid data!" error-menu ": "))))
         (record
           (or record
               (lambda (data) #f)))
         (action
           (or action
               (lambda (data) #f)))
         (choose-next
           (or choose-next
               (lambda (data) 'QUIT))))
    (lambda (previous-result)
      (let loop ((input (get-input)))
        (let* ((vres (validate input))
               (valid (car vres)))
          (if valid
            (let ((data (cdr vres)))
              (record data)
              (action data)
              (list (choose-next data) data))
            (let ((error-choice (get-error-choice)))
              (cond 
                ((string=? error-choice "") (loop (get-input)))
                ((string=? error-choice "s") (cons (choose-next 'NONE) 'NONE))
                ((string=? error-choice "a")
                 (let ((data (cdr vres)))
                   (record data)
                   (action data)
                   (cons (choose-next data) data)))
                ((string=? error-choice "q") (cons 'QUIT 'NONE))))))))))
               
      
;;; ============================================================================



;;; ============================================================================
;;; --  MAIN INTERACTION  ------------------------------------------------------

(define (interact steps #!optional (final-action (lambda (data) #f)))
  (let* ((step-ids
          (map
            (lambda (elt) (car elt))
            steps))
         (get-next
           (lambda (id)
             (let loop ((steps* step-ids)
                        (found #f))
               (cond
                 ((null? steps*) #f)
                 (found (car steps*))
                 ((eqv? id (car steps*)) (loop (cdr steps*) #t))
                 (else (loop (cdr steps*) #f))))))
         (normal-exit
           (lambda (data)
             (final-action data)
             (exit)))
         (start (car step-ids)))
    (let loop* ((step-id start)
                (last-data 'NONE))
      (let ((step (alist-ref step-id steps)))
        (if step
          (let* ((result (step last-data))
                 (signal (car result))
                 (data (cadr result)))
            (case signal
              ((NEXT) (loop* (get-next step-id) data))
              ((LOOP) (if (get-loop-choice data)
                        (loop* start data)
                        (normal-exit data)))
              ((QUIT) (normal-exit data))
              ((ABORT) (error data))
              (else (loop* signal data))))
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
