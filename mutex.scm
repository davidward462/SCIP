(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
	     false)))

(define (clear! cell)
  (set-car! cell false))

(define cell (list true))
