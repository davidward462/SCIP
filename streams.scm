;; Lecture video 6A: Streams

(define (map-stream proc s)
  (if (empty-stream? s)
      the-empty-stream
      (cons-stream
       (proc (head s))
       (map-stream proc (tail s)))))

(define (filter pred s)
  (cond
   ((empty-stream? s) the-empty-stream)
   ((pred (head s))
    (cons-stream (head s)
		 (filter pred
			 (tail s))))
   (else (filter pred (tail s)))))

(define (accumulate combiner init-val s)
  (if (empty-stream? s)
      init-val
      (combiner (head s)
		(accumulate combiner
			    init-val
			    (tail s)))))

(define (enum-tree tree)
  (if (leaf-node? tree)
      (cons-stream tree
		   the-empty-stream)
      (append-streams
       (enum-tree
	(left-branch tree))
       (enum-tree
	(right-branch tree)))))

(define (append-streams s1 s2)
  (if (empty-stream? s1)
      s2
      (cons-stream
       (head s1)
       (append-streams (tail s1)
		       s2))))

(define (enum-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (enum-interval (+ 1 low) high))))


;; Return the sum of the squares of the odd values in a tree.
(define (sum-odd-squares tree)
  (accumulate
   +
   0
   (map
    square
    (filter odd?
	    (enum-tree tree)))))

