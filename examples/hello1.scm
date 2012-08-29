;;; hello1.scm -- A basic 'Hello, World!'-type app using the text-menu egg
;;;   Copyright © 2012 by Matthew C Gushee <matt@gushee.net>

(use text-menu)

(interact
  (steps
    (name: (make-step 'name prompt-msg: "What's your name?" default: "world"))))

(let* ((result (get-all-data))
       (name (cdr (queue-first (queue-first result)))))
  (print "Hello, " name "!"))
