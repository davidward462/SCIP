(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
		 x)))

(define (improve guess x)
  (avg guess (/ x guess)))

(define (avg x y)
  (/ (+ x y) 2))

(define tolerance 0.001)

(define (good-enough? guess x)
  (< (abs (- (square guess) x )) tolerance))

(define (abs x)
  (if (< x 0) (- x) x))

(define (square x)
  (* x x))

(define (sqrt n)
  (sqrt-iter 1.0 n))
