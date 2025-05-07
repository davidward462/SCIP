  ;; check the various conditions.
  ;; d/dx (c) = 0
  ;; d/dx (x) = 1
  ;; d(u + v)/dx = du/dx + dv/dx
  ;; d(uv)/dx = (u)(du/dx) + (dv/dx)(v)
  ;; and so on...)
(define (deriv expr var)
  (newline)
  (display expr)
  (cond ((constant? expr var) 0)
	((same-var? expr var) 1)
	;; if expression is a sum
	((sum? expr)
	 (make-sum (deriv (a1 expr) var)
		   (deriv (a2 expr) var)))
	;; if expression is a product
	((product? expr)
	 (make-sum
	  (make-product (m1 expr)
			(deriv (m2 expr) var))
	  (make-product (deriv (m1 expr) var)
			(m2 expr))))
	;; if expression is an exponentiation
	((exponent? expr)
	 (make-product
	  (make-product (exp expr)
			(make-exponent (base expr) (- (exp expr) 1)))
	  (deriv (base expr) var)))
	
	(else
	 (error "unknown expression type"))))

(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

;; check if the expression is something simple,
;; but is not that variable.
(define (constant? expr var)
  (and (atom? expr)
       (not (eq? expr var))))

(define (same-var? expr var)
  (and (atom? expr)
       (eq? expr var)))

;; a sum is a list whose first element is the symbol +
(define (sum? expr)
  (and (not (atom? expr))
       (eq? (car expr) '+)))

(define (=number? expr num)
  (and (number? expr) (= expr num)))

;; build a summation expression
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list '+ a1 a2))))

;; a1 is the second item of a sum list
(define a1 cadr)

;; a2 is the third item of a sum list
(define a2 caddr)

;; a product is a list whose first element is the symbol *
(define (product? expr)
  (and (not (atom? expr))
       (eq? (car expr) '*)))

;; build a product expression
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list '* m1 m2))))

;; second item in the list
(define m1 cadr)

;; third item in the list
(define m2 caddr)

;; check if expression is an exponentiation
(define (exponent? expr)
  (and (not (atom? expr))
       (eq? (car expr) '**)))

;; create an exponent expression
(define (make-exponent base exp)
  (list '** base exp))

;; get the base of an exponentiation
(define base cadr)

;; get the exponent of an exponentiation
(define exp caddr)

;; do some examples
(define foo
  '(+ (* a (* x x))
      (+ (* b x)
	 c)))
