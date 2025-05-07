;; implement sets, representing them as unordered lists.

;; return true of x is in the set, false otherwise.
(define (elem-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else (elem-of-set? x (cdr set)))))

;; add x into the set
(define (adjoin-set x set)
  (if (elem-of-set? x set)
      set
      (cons x set)))

;; return the intersection of sets s and t
(define (intersection s t)
  (cond ((or (null? s) (null? t)) '())
	((elem-of-set? (car s) t)
	 (cons (car s)
	       (intersection (cdr s) t)))
	(else (intersection (cdr s) t))))

;; return the union of sets s and t
(define (union s t)
  (cond ((null? s) t)
	;; if the first element of s is in t, we don't need to add it.
	((elem-of-set? (car s) t)
	 (union (cdr s) t))
	;; else, the first element of s isn't in t, we add it and recurse.
	(else
	 (cons (car s) (union (cdr s) t)))))
