
;; count the leaves in a tree
(define (count-leaves tree)
  ;; if the tree is nil, return 0
  (cond ((null? tree) 0)
	;; else if the tree is not a pair, then it is a leaf
	((not (pair? tree)) 1)
	(else (+ (count-leaves (car tree))
		 (count-leaves (cdr tree))))))

(define a (cons (list 1 2) (list 3 4)))

(define b (list 1 (list 2 (list 3 4))))

(define c (list 1 3 (list 5 7) 9))

(define d (list (list 7)))

(define e (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(define f (list 1 (list 2 7)))

(define get-e (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr e))))))))))))

(define x (list 1 2 3))

(define y (list 4 5 6))

(define tree-1 (list (list 1 2) (list 3 4)))

(define tree-2 (list 1 (list 2 (list 3 4) 5) (list 6 7)))

;; scale all elements of a tree by the specified factor
(define (scale-tree tree factor)
  (cond ((null? tree) ())
	((not (pair? tree)) (* tree factor))
	(else (cons (scale-tree (car tree) factor)
		    (scale-tree (cdr tree) factor)))))

;; scale a tree using map
(define (scale tree factor)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     ;; if the sub tree is a pair, call it again
	     (scale sub-tree factor)
	     ;; else it's just a number, so multiply it
	     (* sub-tree factor)))
       ;; called in the form: (map lambda... tree)
       tree))

;; square all the leaves in a tree
(define (square-tree tree)
  (cond ((null? tree) ())
	((not (pair? tree)) (* tree tree))
	(else (cons (square-tree (car tree))
		    (square-tree (cdr tree))))))

(define (sqr-tree tree)
  (map (lambda (subtree)
	 (if (pair? subtree)
	     (sqr-tree subtree)
	     (* subtree subtree)))
       tree))

(define (square x)
  (* x x))

(define (tree-map tree f)
  (map (lambda (subtree)
	 (if (pair? subtree)
	     (tree-map subtree f)
	     ;; else
	     (f subtree)))
       tree))
