(use redis-client)

(include "text-menu.scm")

(define *current-category* (make-parameter "metals"))

(define (init)
  (redis-connect "localhost" 6379)
  (let ((item #f))
    (*recorder*
      (lambda (arg . args)
        (cond
          ((eqv? arg 'set-item)
           (set! item (car args)))
          ((eqv? arg 'get) 
           #f) ; need to build a list of all items here
          ((null? args)
           (redis-hget item arg))
          (else
           (redis-hset item arg (car args))))))))


(define-enum 'vendors extensible: #t)

(define-enum 'categories elts: '("metals" "stones" "parts" "tools" "supplies" "admin"))

(define-enum 'materials extensible: #t elts: '("Ag925" "Cu110" "Argentium" "AgFine"))

(define-enum 'metal-forms extensible: #t elts: '("sheet" "tube" "wire"))

(define-enum 'metal-shapes extensible: #t elts: '("round" "square" "rectangular"))

(define-enum 'stone-forms extensible: #t elts: '("cabochon" "faceted" "bead" "raw"))

(define-enum 'stone-shapes extensible: #t elts: '("round" "ellipse" "square" "rect" "triangle" "trapezoid" "irregular"))

(define-step 'category
             type: (enum categories)
             default: (*current-category*)
             next: 'purchase-date)

(define-step 'purchase-date
             type: string
             validator: date
             next: 'vendor)

(define-step 'vendor
             type: (enum vendors)
             next: 'refnum)

(define-step 'refnum
             type: string)
