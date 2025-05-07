(define (exp b n)
  (if (= n 0)
      1
      (* b (exp b (- n 1)))))

(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
		 (- counter 1)
		 (* b product))))

(define (square x)
  (* x x))

(define (even? n)
  (= (remainder n 2) 0))

(define (^ b n)
  (cond ((= n 0) 1)
	((even? n) (square (^ b (/ n 2))))
	(else (* b (^ b (- n 1))))))
