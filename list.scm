(define mylist (list 1 2 3 4))

;; same as list-ref. Return the list item from index i.
(define (list-get items i)
  (if (= i 0)
      (car items)
      (list-get (cdr items) (- i 1))))

;; test list-get
(define (test-list-get items i)
  (equal? (list-get items i) (list-ref items i)))

;; define new function for length
(define (len items)
  (if (null? items)
      0
      (+ 1 (len (cdr items)))))

;; test len
(define (test-len items)
  (equal? (length items) (len items)))

;; reverse a list
;; There is a more efficient method also.
(define (reverse items)
  (if (null? items)
      ()
      (append (reverse (cdr items)) (list (car items)))))

;; reverse a list and all of its sublists
(define (deep-reverse items)
  (if (null? items)
      ()
      (append (deep-reverse (cdr items))
	      (list (if (pair? (car items))
			(deep-reverse (car items))
			(car items))))))

;; test deep-reverse
(define (test-dr input expected)
  (equal? (deep-reverse input) expected))

;; definition of map
(define (my-map proc items)
  (if (null? items)
      ()
      (cons (proc (car items))
	    (my-map proc (cdr items)))))

(define (scale-list items factor)
  (map (lambda (x) (* x factor)) items))

;; return the square of a number
(define (square x)
  (* x x))

;; square all of the elements in a list
(define (squ-list items)
  (if (null? items)
      ()
      (cons (square (car items)) (squ-list (cdr items)))))

;; square all elements in a list
(define (square-list items)
  (map square items))

;; iterate over all items in a list but don't return anything in particular
(define (for proc items)
  (map proc items)
  0)
