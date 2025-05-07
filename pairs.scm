;; Implementation of cons called 'pair'
;; Pair returns a procedure, with a and b substituted in.
(define (pair a b)
  (lambda (pick)
    (cond ((= pick 1) a)
	  ((= pick 2) b))))

;; Implementation of car called 'first'.
;; 'first' calls the function it was passed with the value of 1.
;; This would call the function returned from pair with the argument 1.
(define (first x)
  (x 1))

;; Implementation of cdr called 'second'
;; 'second' calls the function it was passed with the value of 2.
(define (second x)
  (x 2))



