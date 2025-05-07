(define (use-function f x)
  (f x))

(define (local)
  (let ((a 1)
	(b 10)
	(c 100))
    (+ a b c)))

(define (f g)
  (g 2))

(define (square n)
  (* n n))
