(define (sum a b)
  (if (> a b)
      0
      (+ a (sum (+ a 1) b))))

(define (square x)
  (* x x))

(define (sum-sq a b)
  (if (> a b)
      0
      (+ (square a) (sum-sq (+ a 1) b))))

(define (sigma start end next func)
  (if (> start end)
      0
      (+ (func start)
	 (sigma (next start) end next func))))

(define (sum-cube a b)
  (define (cube n) (* n n n))
  (sigma a b 1+ cube))

(define (sum-tens a b)
  (define (id a) a)
  (sigma a b (lambda (x) (+ x 10)) id))

(define (sum-linear a b)
  (sum-iter a b 0))

(define (sum-iter start end total)
  (if (> start end)
      total
      (sum-iter (+ start 1) end (+ total start))))
