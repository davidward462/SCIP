# 1.41

(define (inc n)
  (+ n 1))

(define (double f)
  (lambda (x) (f (f x))))

# 1.42

(define (compose f g)
  (lambda (x) (f (g x))))

(define (square x)
  (* x x))

# 1.43

(define (repeated f n)
  (if (= n 0)
      (lambda (x) x)
      (compose f (repeated f (- n 1)))))
