(define (hello-world)
  (display "hello world"))

(define (average a b)
  (/ (+ a b) 2.0))

(define (average-damp f)
  (lambda (x) (average x (f x))))

