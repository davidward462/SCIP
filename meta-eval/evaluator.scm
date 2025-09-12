;; The Metacircular Evaluator
;; Ch 4.1

;; Procedure Arguments
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
	    (list-of-values (rest-operands exps) env))))

;; Conditionals
;; 'if-predicate' is evaluated in the lanaguage we are implementing (which is a subset of Scheme) and so
;; it has a value in that language.
;; 'true?' checks if the value in the interpreted langauge is what we consider to be true in our implementation
;; language (which in this case is the full Scheme language).
;; 'true?' is the interface here between the two languages.
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;; Sequences
;; This is used by
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
	(else (eval (first-exp exps) env)
	      (eval-sequence (rest-exps exps) env))))

;; Assignment
;; Install the value and variable in the designated environment.
;; The choice of returning 'ok' is arbitrary.
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
		       (eval (assignment-value exp) env)
		       env)
  'ok)


;; TODO: define-variable! is not being recognized as a procedure I think.
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

;; Eval
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


