(define (average a b)
  (/ (+ a b) 2.0))

(define epsilon 0.00001)

(define (fixed-point f first-guess)
  (define (close? v1 v2)
    (< (abs (- v1 v2)) epsilon))
  (define (try guess)
    (let ((next (f guess)))
      (if (close? guess next)
	  next
	  (try next))))
  (try first-guess))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
	       1.0))

(define (phi)
  (fixed-point (lambda (x) (+ 1.0 (/ 1.0 x)))
	       1.0))
