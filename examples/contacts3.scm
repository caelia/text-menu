(use text-menu)
(use redis-client)

(redis-connect "localhost" 6379)
(redis-select "1")   ; This is just for my own purposes, on my own box

(define (next-id)
  (car (redis-incr "next-id")))

(define redis-recorder
  (let ((key #f))
    (lambda (arg . args)
      (cond 
        ((equal? arg 'set-key!)
         (set! key (sprintf "/people/~A" (car args))))
        (else
         (let ((field (symbol->string arg))
               (value (car args)))
           (redis-hset key field value)))))))

(define (reset)
  (redis-recorder 'set-key! (next-id)))

(redis-recorder 'set-key! (next-id))

(interact
  (steps
    (first: (make-step 'first record: redis-recorder))
    (last: (make-step 'last record: redis-recorder))
    (phone: (make-step 'phone required: #f record: redis-recorder))
    (email: (make-step 'email record: redis-recorder)))
  looping: #t
  after-iteration: reset)
