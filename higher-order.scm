(define (sigma a b f next)
  (if (> a b)
      0
      (+ (f a)
	 (sigma (next a) b f next))))

(define (inc n)
  (+ n 1))

(define (identity n)
  n)

(define (square n)
  (* n n))

(define (cube n)
  (* n n n))

(define (sum-cubes a b)
  (sigma a b cube inc))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sigma (+ a (/ dx 2.0)) b f add-dx)
     dx))

(define (sigma-iter a b term next)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ result (term a)))))
  (iter a 0))

