;; From lecture video 5B: Computational objects.

;; These procedures demonstrate that once the 'set!' procedure is introduced,
;; then all other assignment procedures follow (including 'set-car!' and 'set-cdr!'.

(define (my-cons x y)
  (lambda (m)
    (m x
       y
       (lambda (n) (set! x n))
       (lambda (n) (set! y n)))))

(define (my-car x)
  (x (lambda (a d sa sd) a)))

(define (my-cdr x)
  (x (lambda (a d sa sd d))))

(define (my-set-car! x y)
  (x (lambda (a d sa sd) (sa y))))

(define (my-set-cdr! x y)
  (x (lambda (a d sa sd) (sd y))))

;; Below, note that x and y have the values of 1 and 2 in their environments. Therefore the symbol 'x' actually has a value where you see it (the value of 1).

;; (define k (cons 1 2))
;; (my-set-car k 10)
;; (k (lambda (a d sa sd) (sa 10)))
;; ((lambda (m) (m x y (lambda (n) (set! x n)) (lambda (n) (set! y n)))) (lambda (a b sa sd) (sa 10)))
;; ((lambda (n) (set! x n)) 10)
;; (lambda (10) (set! x 10))
;; (set! x 10)
