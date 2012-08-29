;;; hello2.scm -- Another 'Hello, World!'-type app using the text-menu egg
;;;   Copyright © 2012 by Matthew C Gushee <matt@gushee.net>

(use text-menu)

(define get-name
  (make-step 'name
             prompt-msg: "What's your name?"
             default: "world"
             action: (lambda (result) (print "Hello, " result "!"))))

(interact (steps get-name))
