(use text-menu)

(interact
  (steps
    (first: (make-step 'first))
    (last: (make-step 'last))
    (phone: (make-step 'phone required: #f))
    (email: (make-step 'email))
    (lastcon: (make-date-step 'last-contact))
    (bd: (make-date-step 'birthday default-year-rule: 'none default-month-rule: 'none)))
  looping: #t)

(pp (map queue->list (queue->list (get-all-data))))
