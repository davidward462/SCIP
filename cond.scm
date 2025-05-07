(define (inc val) (+ val 1))

(define (abs x)
  (cond ((> x 0) x)
	((= x 0) 0)
	((< x 0) (- x))))

(define (are_equal a b)
  (cond ((= a b) 1)
	(else 0)))

(define (is_nine x)
  (if (= x 9)
      1
      0))

(define (equal_sum a b sum)
  (if (= (+ a b) sum) 1 0))

(define (>= a b)
  (or (= a b) (> a b)))

(define (<= a b)
  (or (= a b) (< a b)))

(/ (+ 5
      4
      (- 2
	 (- 3
	    (+ 6 (/ 4
		    5)))))
   (* 3
      (- 6
	 2)
      (- 2
	 7)))
    
