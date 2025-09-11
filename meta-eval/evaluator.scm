;; The Metacircular Evaluator
;; Ch 4.1


;; 'eval' contains the rules used to evaluate the given expression 'exp' in the environment 'env'.
;; The overall structure of the procedure is using a case analysis with 'cond'.
;; Note that to add new types of expressions to this language, we would need to directly edit the 'eval' procedure below.
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-var-value exp env))
	((quoted? exp) (text-of-quotation exp))
	((assignment? exp) (eval-assignment exp env))
	((definition? exp) (eval-definition exp env))
	((if? exp) (eval-if exp env))
	((lambda? exp)
	 (make-procedure (lambda-params exp)
			 (lambda-body exp)
			 env))
	((begin? exp)
	 (eval-sequence (begin-actions exp) env))
	((cond? exp) (eval (cond->if exp) env))
	((application? exp)
	 (apply (eval (operator exp) env)
		(list-of-values (operands exp) env)))
	(else
	 (error "Unknown expression type -- EVAL" exp))))

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
