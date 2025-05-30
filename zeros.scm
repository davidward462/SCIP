(define (linear x)
  x)

(define (close-enough? a b)
  (< (abs (- a b)) 0.001))

(define (positive? n)
  (> n 0))

(define (negative? n)
  (< n 0))

(define (average a b)
  (/ (+ a b) 2.0))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
	midpoint
	(let ((test-value (f midpoint)))
	  (cond ((positive? test-value)
		 (search f neg-point midpoint))
		((negative? test-value)
		 (search f midpoint pos-point))
		(else midpoint))))))

(define (half-interval f a b)
  (let ((a-value (f a))
	(b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
	   (search f a b))
	  ((and (negative? b-value) (positive? a-value))
	   (search f b a))
	  (else
	   (error "Values are not of opposite signs" a b)))))

(define (f g x)
  (g x))
