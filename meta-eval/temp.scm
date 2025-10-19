(define (greater a b) (if (> a b) a b))

(define (less a b) (cond ((< a b) a) ((< b a) b) (else 'equal)))


;; Save reference to the oringinal 'apply' procedure
(define apply-in-underlying-scheme apply)

;; Apply
;; 'apply' takes a procedure and a list or arguments.
;; It handles primitive procedures and compound procedures differently, in the 'cond' statement.
;; For the compound procedure, the environment of the given procedure must be extended so that the arguments can be bound to the parameters.
(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
	 (apply-primitive-procedure procedure arguments))
	((compound-procedure? procedure)
	 (eval-sequence
	  (procedure-body procedure)
	  (extend-environment
	   (procedure-parameters procedure)
	   arguments
	   (procedure-environment procedure))))
	(else
	 (error "Unknown procedure type -- APPLY" procedure))))

