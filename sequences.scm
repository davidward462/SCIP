(define cardinal (list 1 2 3 4 5 6 7 8 9))
(define t (list (list 1 2) (list 3 4)))
(define t1 (list 1 (list 2 (list 3 4) 5)))

(define (square n)
  (* n n))

;; Filter a list.
;; Return a list containing only elements from the sequence which
;; are true according to the predicate.
(define (filter predicate sequence)
  (cond ((null? sequence) ())
	((predicate (car sequence))
	 (cons (car sequence)
	       (filter predicate (cdr sequence))))
	(else (filter predicate (cdr sequence)))))

;; Accumulate items from a sequence, using the given operation and initial value.
(define (acc op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (acc op initial (cdr sequence)))))

(define (enum-interval low high)
  (if (> low high)
      ()
      (cons low (enum-interval (+ low 1) high))))

;; Enumerate all the leaves of a tree.
;; Same as the 'fringe' procedure.
(define (enum-tree tree)
  (cond ((null? tree) ())
	((not (pair? tree)) (list tree))
	(else (append (enum-tree (car tree))
		      (enum-tree (cdr tree))))))

;; Enumerate through the leaves of a tree,
;; take only the odd numbers,
;; square each number,
;; and sum all of the results.
(define (sum-odd-squares tree)
  (acc +
       0
       (map square
	    (filter odd?
		    (enum-tree tree)))))

(define (prod-of-squares-of-odd-elements sequence)
  (acc *
       1
       (map square
	    (filter odd? sequence))))

(define (sum-of-even-elements sequence)
  (acc +
       0
       (map (lambda (x) x)
	    (filter even? sequence))))

;; Append s2 onto s1
(define (my-append s1 s2)
  (acc cons s2 s1))



