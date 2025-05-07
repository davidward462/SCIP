(define a 1)

(define b 2)

(define l (list a b))

(define m (list 'a 'b))

(define empty '())

;; Take as arguments a symbol and a list.
;; If the symbol is found in the list, return the list,
;; else do recursion on the cdr of the list.
(define (memq symbol list)
  (cond ((null? list) false)
	((eq? symbol (car list)) list)
	(else (memq symbol (cdr list)))))

(define (are-equal? s t)
  (cond
   ;; if both lists are null (we have reached the end)
   ((and (null? s) (null? t)) true)
   ;; if only one is null
   ((or (null? s) (null? t)) false)
   ;; if their cars are not equal
   ((not (eq? (car s) (car t))) false)
   ;; else do the recursion on the cdrs.
   (else (are-equal? (cdr s) (cdr t)))))


(define (test-equal s t)
  (eq? (are-equal? s t) (equal? s t)))
