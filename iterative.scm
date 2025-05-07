(define (for f arg n)
  (if (= n 0)
      (f arg)
      (for f arg (- n 1))))

(define (print arg)
  (newline)
  (display arg))

(define (scale items factor)
  (if (null? items)
      ()
      (cons (* factor (car items)) (scale (cdr items) factor))))

(define (my-map proc l)
  (if (null? l)
      ()
      (cons (proc (car l)) (my-map proc (cdr l)))))

;; definition for for-each (general for loop).
;; cond and else are used so that multiple statements can be executed in
;; the else clause. "if" does not work for this case.
(define (for f list)
  (cond ((null? list)
	 "done")
	(else (f (car list))
	      (for f (cdr list)))))
