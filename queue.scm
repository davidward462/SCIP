;; 3.3.2 Representing Queues

(define (front-ptr q) (car q))

(define (rear-ptr q) (cdr q))

(define (set-front-ptr! q item)
  (set-car! q item))

(define (set-rear-ptr! q item)
  (set-cdr! q item))

(define (make-queue)
  (cons '() '()))

;; The queue is considered empty if the front pointer is null.
(define (empty-queue? q)
  (null? (front-ptr q)))
   
;; Select the item at the front of the queue by returning the car of the pair
;; indicated by the front pointer.
(define (front-queue q)
  (if (empty-queue? q)
      (error "pop called with empty queue" q)
      (car (front-ptr q))))

;; Insert an item at the rear of the queue
(define (insert-queue! q item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? q)
	   (set-front-ptr! q new-pair)
	   (set-rear-ptr! q new-pair)
	   q)
	  (else
	   (set-cdr! (rear-ptr q) new-pair)
	   (set-rear-ptr! q new-pair)
	   q))))

;; To delete from the front of the queue, move the front pointer so it points at
;; the second item.
(define (delete-queue! q)
  (cond ((empty-queue? q)
	 (error "delete called with empty queue" q))
	(else
	 (set-front-ptr! q (cdr (front-ptr q)))
	 q)))

(define q (make-queue))
