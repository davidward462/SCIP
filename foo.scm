(define (foo x y)
  (define (bar a)
    (* a x))

  (define (baz b)
    (+ b x))

  (+ (bar x) (baz y)))
