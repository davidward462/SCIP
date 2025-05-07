
(define (square x)
  (* x x))

(define dx 0.0000001)

(define (sqrt x)
  (newton (lambda (y) (- x (square y)))
	  1))

(define (newton f guess)
  (define df (derivative f))
  (fixed-point
   (lambda (x) (- x (/ (f x) (df x))))
   guess))n

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define (cube x)
  (* x x x))

(define (linear x)
  (+ x 0))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
