(define (sum-int a b)
  (if (> a b)
      0
      (+ a (sum-int (1+ a) b))))

