(use text-menu)

(interact
  (steps
    (first: (make-simple-step 'first))
    (last: (make-simple-step 'last))
    (email: (make-simple-step 'email))
    (phone: (make-simple-step 'phone required: #f)))
  looping: #t)

(pp (map queue->list (queue->list (get-all-data))))
