

;; procedure p halts.
(define (p a)
  a)

;; procedure p-prime does not halt.
(define (p-prime a)
  (p-prime a))

;; Here we assume (incorrectly, as seen above) that every program halts.
(define (halts? p a)
  true)

;; This procedure does not halt.
(define (run-forever)
  (run-forever))

(define (try p)
  (if (halts? p p)
      (run-forever)
      'halted))
