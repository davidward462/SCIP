; create a vector (pair of values)
(define (make-vector x y)
  (cons x y))

; return the x coordinate of a vector
(define (xcord v)
  (car v))

; return the y coordinate of a vector
(define (ycord v)
  (cdr v))

; create a line segment
(define (make-segment p q)
  (cons p q))

; return the starting coordinates of a line segment
(define (seg-start s)
  (car s))

; return the ending coordinates of a line segment
(define (seg-end s)
  (cdr s))

; determine the average of two numbers
(define (average a b)
  (/ (+ a b) 2))

; return the midpoint of a line segment
(define (midpoint s)
  (let ((a (seg-start s))
	(b (seg-end s)))
    (make-vector
     (average (xcord a) (xcord b))
     (average (ycord a) (ycord b)))))

; square a number
(define (square n)
  (* n n))

; return the length of a line segment
(define (length s)
  (let
      ((dx (- (xcord (seg-end s))
	      (xcord (seg-start s))))
       (dy (- (ycord (seg-end s))
	      (ycord (seg-start s)))))
    (sqrt (+ (square dx)
	     (square dy)))))
  
