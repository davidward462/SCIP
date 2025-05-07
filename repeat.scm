(define (inc n)
  (+ n 1))

;; return a procedure which calls f a number of times equal to n.
(define (repeat f n)
  (if (= n 1)
      ;; if n is 1, return f
      f
      ;; else
      (repeat (lambda(x) (f (f x))) (- n 1))))
