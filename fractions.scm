(car (cons 2 8))
; 2

(cdr (cons 10 3))
; 3

(define p1 (cons 0 0))

(define p2 (cons 1 5))

;; create a rational number.
;; Normalize the negative signs, if any.
(define (make-rat n d)
  (let ((g (gcd n d)))
    (let ((n (/ n g))
	  (d (/ d g)))
      (if (< d 0)
	(cons (- n) (- d))
	(cons n d)))))


(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

;; add two rational numbers
(define (+rat a b)
  (make-rat (+ (* (numer a) (denom b))
	       (* (numer b) (denom a)))
	    (* (denom a) (denom b))))

;; print a rational number to the display
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))
