(use text-menu)

(interact
  (steps
    (first: (make-step 'first))
    (last: (make-step 'last))
    (phone: (make-step 'phone
                       required: #f
                       validate: (make-regex-validator '(: (= 3 numeric) #\- (= 3 numeric) #\- (= 4 numeric))
                                                       "The number must be 10 digits with dashes, e.g. 123-456-7890")))
    (email: (make-step 'email)))
  looping: #t)

(pp (map queue->list (queue->list (get-all-data))))
