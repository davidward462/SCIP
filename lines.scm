;; construct a point
(define (make-point x y)
  (cons x y))

;; select the x coordinate of a point
(define (x-point p)
  (car p))

;; select the y coordinate of a point
(define (y-point p)
  (cdr p))

;; print a point
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;; construct a line
(define (make-segment s t)
  (cons s t))

;; select the start of a line segment
(define (start-segment s)
  (car s))

;; select the end of a line segment
(define (end-segment s)
  (cdr s))

;; print a line segment
(define (print-seg s)
  (newline)
  (let ((start (start-segment s))
	(end (end-segment s)))
    (let ((sx (x-point start))
	  (sy (y-point start))
	  (ex (x-point end))
	  (ey (y-point end)))
      (newline)
      (display "((")
      (display sx)
      (display ",")
      (display sy)
      (display ") ")
      (display "(")
      (display ex)
      (display ",")
      (display ey)
      (display "))"))))
