
(define (f x)
  (* x x))

(define (fixed-point f start)
  (define tolerance 0.00001)
  (define (close? u v)
    (< (abs (- u v)) tolerance))
  (define (iter old new)
    (if (close? old new)
	new
	(iter new (f new))))
  (iter start (f start)))

(define (sqrt x)
  (fixed-point
   (avg-damp (lambda (y) (/ x y)))
   1))

(define avg-damp
  (lambda (f)
    (lambda (x) (average (f x) x))))
