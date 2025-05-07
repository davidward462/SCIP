;; add interval x and y together
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

;; subtract interval y from x
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
		 (- (upper-bound x) (upper-bound y))))

;; multiply intervals x and y
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-inverval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))


;; define interval x by interval y
(define (div-interval x y)
  (if (< (lower-bound y) 0)
      ;; y spans 0
      (error "second interval spans zero.")
      (mul-interval x
		    (make-interval (/ 1.0 (upper-bound y))
				   (/ 1.0 (lower-bound y))))))

;; create an interval
(define (make-interval a b)
  (cons a b))

;; select the upper bound of the interval
(define (upper-bound x)
  (max (car x) (cdr x)))

;; select the lower bound of the interval
(define (lower-bound x)
  (min (car x) (cdr x)))

;; values for testing
(define a (make-interval 1.0 2.0))
(define b (make-interval 0.1 0.2))
(define c (make-interval -1.5 1.4))
