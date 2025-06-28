;; From lecture 5B: Computational objects

;; For testing only
(define (f)
  (display "f")
  0)

(define (g)
  (display "g")
  0)

(define (h)
  (display "h")
  0)

(define (i)
  (display "i")
  0)

(define p (list f g h i))

;; Call procedures from a list 'procs'
(define (call-each procs)
  (cond ((null? procs) 'done)
	(else
	 ((car procs))
	 (call-each (cdr procs)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-proc)
  ((wire 'add-action!) action-proc))

(define (after-delay delay action)
  (add-to-agenda!
   (+ delay (current-time the-agenda))
   action
   the-agenda))

(define (propagate)
  (cond ((empty-agenda? the-agenda)
	 'done)
	(else
	 ((first-item the-agenda))
	 (remove-first-item! the-agenda)
	 (propagate))))

;; This is a "message accepting object"
(define (make-wire)
  (let ((signal 0) (action-procs '()))
    (define (set-my-signal! new)
      (cond ((= signal new) 'done)
	    (else
	     (set! signal new)
	     (call-each action-procs))))
    (define (accept-action-proc proc)
      (set! action-procs
	    (cons proc action-procs))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal)
	    ((eq? m 'set-signal!)
	     set-my-signal!)
	    ((eq? m 'add-action!)
	     accept-action-proc)
	    (else
	     (error "bad message" m))))
    dispatch))

(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

;; Negation
(define (logical-not s)
  (cond ((= s 0) 1)
	((= s 1) 0)
	(else
	 (error "invalid signal" s))))

;; NOT gate
(define (inverter input output)
  (define (invert-in)
    (let ((new-value
	   (logical-not (get-signal input))))
      (after-delay inverter delay
		   (lambda ()
		     (set-signal! output new-value)))))
  (add-action! in invert-in))

(define (logical-and a1 a2)
  (if (and (= a1 1) (= a2 1))
      1
      0))
  
;; AND gate
(define (and-gate a1 a2 output)
  (define (and-action-proc)
    (let ((new-value
	   (logical-and (get-signal a1)
			(get-signal a2))))
      (after-delay and-gate-delay
		   (lambda ()
		     (set-signal! output new-value)))))
  (add-action! a1 and-action-proc)
  (add-action! a2 and-action-proc))

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b d)
    (inverter c e)
    (and-gate d e s)))

;; See how the computational objects we have created
;; can be used as if they are primitives.
(define (full-adder a b c-in sum c-out)
  (let ((s make-wire)
	(c1 (make-wire))
	(c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)))
