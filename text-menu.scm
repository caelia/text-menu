(use input-parse)

(module text-menu *

        (import scheme)
        (import chicken)
        (import srfi-13)
        (import data-structures)
        (import input-parse)
        (import yasos)


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

(define (get-input message #!optional (default #f))
  (let* ((default-string
           (cond
             ((not default) "")
             ((eqv? default 'yes) " [Y/n]")
             ((eqv? default 'no) " [y/N]")
             (else (string-append " [" default "]"))))
         (prompt-string (string-append message default-string ": ")))
    (display prompt-string)
    (let ((input (string-trim-both (read-text-line))))
      (if (string=? input "")
        (cond
          ((not default) "")
          ((eqv? default 'yes) "y")
          ((eqv? default 'no) "n")
          (else default))
        input))))

(define (get-loop-choice data)
  (let ((custom-fun (*custom-loop-choice-function*)))
    (if custom-fun
      (custom-fun data)
      (let ((input (get-input "Finished one round. Press [RETURN] to start again, or 'q' to quit.")))
        (not (string=? (string-downcase input) "q"))))))

;;; ============================================================================



;;; ============================================================================
;;; --  INTERACTION MODULE: BASE OBJECT  ---------------------------------------
;;;
;;; --  Interface  -------------------------------------------------------------

(define-predicate imod?)
(define-operation (execute imod data))

;;; --  Implementation  --------------------------------------------------------

(define (imod #!key input-fun validator recorder action choose-next (required #t) (allow-override #f))
  (object
    ((get-input self) (input-fun)
    ((validate self data) (validator data))
    ((record self data) (recorder data))
    ((do-action self data) (action data))
    ((choose-next self data) (choose-next data))
    ((execute self prev-data)
     (let loop ((input (get-input self)))
       (let* ((vres (validate self input))
              (valid (car vres)))
         ))))))
      
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
          (let* ((result (execute step last-data))
                 (directive (car result))
                 (data (cadr result)))
            (case directive
              ((NEXT) (loop* (get-next step-id) data))
              ((LOOP) (if (get-loop-choice data)
                        (loop* start data)
                        (normal-exit data)))
              ((QUIT) (normal-exit data))
              ((ABORT) (error data))
              (else (loop* directive data))))
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


;;; ============================================================================
;;; ----------------------------------------------------------------------------
;;; --  Interface  -------------------------------------------------------------
;;; --  Implementation  --------------------------------------------------------
;;; ============================================================================
