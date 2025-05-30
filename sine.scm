(define (cube x) (* x x x))

(define (p x) (- (* 3.0 x) (* 4.0 (cube x))))

(define (abs x)
  (if (< x 0) (- x) x))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))
