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


(register-enum 'vendors extensible: #t)

(register-enum 'categories elts: '("metals" "stones" "parts" "tools" "supplies" "admin"))

(register-enum 'materials extensible: #t elts: '("Ag925" "Cu110" "Argentium" "AgFine"))

(register-enum 'metal-forms extensible: #t elts: '("sheet" "tube" "wire"))

(register-enum 'metal-shapes extensible: #t elts: '("round" "square" "rectangular"))

(register-enum 'stone-forms extensible: #t elts: '("cabochon" "faceted" "bead" "raw"))

(register-enum 'stone-shapes extensible: #t elts: '("round" "ellipse" "square" "rect" "triangle" "trapezoid" "irregular"))

(register-enum 'units extensible: #t elts: '("in" "sq-in" "ft" "sq-ft" "mm" "sq-mm" "cm" "sq-cm" "lb" "oz" "ozt"))

(set-step! 'category
           type: '(enum categories)
           default: (*current-category*)
           next: 'purchase-date-year)

(set-step! 'purchase-date-year
           type: 'string
           validator: 'date
           next: 'vendor)

(set-step! 'vendor
           type: '(enum vendors)
           next: 'refnum)

(set-step! 'refnum
           type: 'string
           branch: (lambda (resp)
                     (let ((category (*current-category*)))
                       (if (or (eqv? category "metals") (eqv? category "stones"))
                         'material
                         'item-type))))

(set-step! 'material
           type: '(enum materials)
           branch: (lambda (resp)
                     (cond
                       ((eqv? category "metals") 'metal-form)
                       ((eqv? category "stones") 'stone-form))))

(set-step! 'metal-form
           type: '(enum metal-forms)
           next: 'metal-shape)

(set-step! 'metal-shape
           required: #f
           type: '(enum metal-shapes))

(set-step! 'stone-form
           type: '(enum stone-forms)
           next: 'stone-shape)

(set-step! 'stone-shape
           type: '(enum stone-shapes)
           next: 'stone-color)

(set-step! 'stone-color
           type: 'string
           next: 'quantity)

(set-step! 'item-type
           type: 'string
           next: 'quantity)

(set-step! 'unit
           type: '(enum units)
           next: 'quantity)

(set-step! 'quantity
           type: 'number
           next: 'lot-price)

(set-step! 'lot-price
           type: 'float
           next: 'notes)

(set-step! 'notes
           required: #f
           type: 'string
           next: 'LOOP)


(set-start 'category)
