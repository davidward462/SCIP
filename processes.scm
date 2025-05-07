(define (! n)
  (if (= n 1)
      1
      (* n (! (- n 1)))))


(define (^ b p)
  (if (= p 1)
      b
      (* b (^ b (- p 1)))))

(define (fact n)
  (define (iter product counter)
    (if (> counter n)
	product
	(iter (* counter product)
	      (+ counter 1))))
  (iter 1 1))

(define (factorial  n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
		 (+ counter 1)
		 max-count)))

(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n 1))
		 (fib (- n 2))))))

(define (fibonacci n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count  0)
      b
      (fib-iter (+ a b) a (- count 1))))
