;;; dumfm.scm -- A dumb file manager
;;;   Copyright © 2012 by Matthew C. Gushee

;;; This program demonstrates two key features of the text-menu library:
;;;  1) Enumerations, which produce menus
;;;  2) Customized action procedures.
 
(use text-menu)

(define (check-source src-file)
  (let* ((data (get-current-data))
         (op (alist-ref 'operation data)))
    (case op
      ((copy
         ;;; XXX This is not going to work. Need to revise enums to allow for symbol + (human-readable) label.
    (and (file-exists? src-file)
         (file-read-access

(define operation-enum (make-enum '("Copy a file or directory" "Move a file or directory" "Delete a file or directory")))

(interact
  (steps
    (operation: (make-enum-step 'operation operation-enum)))
  looping: #t)

(pp (map queue->list (queue->list (get-all-data))))
