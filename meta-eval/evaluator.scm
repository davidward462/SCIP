;; The Metacircular Evaluator
;; Ch 4.1

;; Self-evaluating items like numbers and strings.
(define (self-evaluating? exp)
  (cond ((number? exp) true)
	((string? exp) true)
	(else false)))

;; Variables are symbols.
(define (variable? exp) (symbol? exp))
;; Quotations have the form (quote <text of quotation>)
(define (quoted? exp)
  (tagged-list? exp 'quote))

;; Recall that (cadr x) is the same as (car (cdr x)).
(define (text-of-quotation exp) (cadr exp))

;; The tagged-list? procedure is how we determine the type of the expression usually.
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;; Assignments have the form (set! <var> <value>).
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

;; Definitions have the form (define <var> <value>) or (define (<var> <param 1> ... < param n>) <body>).
;; Recall that 'define' for a procedure is the same as defining a variable as a lambda expression:
;; (define <var> (lambda (<param 1> ... <param n>) <body>))
(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) ;; formal parameters
		   (cddr exp)))) ;; body
	       
;; Lambda expressions are in the form (lambda (<param 1> ... <param n>) <body>).
(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;; Conditionals are in the form (if (<predicate>) <consequent> <alternative>), where the alternative is optional.
(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

;; This is used to transform 'cond' expressions to 'if' expressions.
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


;; 'begin' combines several expressions into a single sequence of expressions in their given order.
(define (begin? exp)
  (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

;; Transform a sequence into a single expression.
(define (sequence->exp seq)
  (cond ((null? seq) seq)
	((last-exp? seq) (first-exp seq))
	(else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

;; Procedure application

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

;; Procedure Arguments
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
	    (list-of-values (rest-operands exps) env))))

;; Derived expressions
;; A 'cond' expression is just a certain arragement of an 'if' expression. Therefore the 'cond' is a derived expression.
;; We can handle it and convert it to an 'if' expression.
(define  (cond? exp) (tagged-list? exp 'cond))

;; The clauses are everything after the 'cond' tag
(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

;; A clause has a predicate (the thing which can be true or false) and an action.
(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

;; We convert the 'cond' to an 'if' by expanding the clauses in the appropriate way.
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

;; This changes a 'cond' expression into a nested set of 'if' expressions, which will the be evaluated.
(define (expand-clauses clauses)
  (if (null? clauses)
      'false           ; in this case there is no 'else' clause.
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(if (cond-else-clause? first)       ; if the 'else' clause is the first one 
	    (if (null? rest)                      ; if there is not any other clause
		(sequence->exp (cond-actions first))
		(error "ELSE clause isn't last -- COND->IF" clauses))
	    (make-if (cond-predicate first) ; the else clause is not the first one
		     (sequence->exp (cond-actions first))
		     (expand-clauses rest))))))


;; True is anything that is not false
;; For example: (true? (= 1 1)) returns true.
(define (true? x)
  (not (eq? x false)))

;; False is anything that is false.
(define (false? x)
  (eq? x false))

;; temporary procedures
(define (apply-primitive-procedure proc args)
  (proc args))

(define (primitive-procedure? proc)
  false)

;; Compound procedures

;; Construct a procedure out of params, body, and environment.
;; The first element is the tag.
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

;; Params is the second element.
(define (procedure-parameters p)
  (cadr p))

;; Body is the third element.
(define (procedure-body p)
  (caddr p))

;; Environment is the fourth element.
(define (procedure-environment p)
  (cadddr p))

;; Environments

;; The enclosing environment is the next one in the list.
(define (enclosing-environment env)
  (cdr env))

(define (first-frame env)
  (car env))

(define the-empty-environment '())

;; Each frame of an environment is a pair of lists: the variables and their values.
(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame)
  (car frame))

(define (frame-values frame)
  (cdr frame))

;; Add a new variable and value to the front of the frame.
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

;; Extend the environment with a new frame which consists of the variable and
;; values that we pass in. It will be a sub-environment of the base environment.
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      ;; if the lengths are not equal
      (if (< (length vars) (length vals))
	  (error "Too many arguments supplied" vars vals)
	  (error "Too few arguments supplied" vars vals))))

;; Lookup a variable name in an environment to find the value.
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     ;; If we didn't find the variable, go to the enclosing environment.
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     (car vals)) ;; if we match the variable, return the value.
	    ;; Continue searching the current frame.
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	;; If the environment is empty, the variable is unbound.
	(error "Unbound variable" var)
	;; else, check the next frame in the list
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))


;; To set a variable, we scan for the variable, like in lookup-variable-value.
;; If we find it, we set the value to the one provided. If we don't find it then
;; it is unbound.
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     (set-car! vals val))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable -- SET!" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))

;; Define a varible.
;; Check the first frame to see if the binding is there. If it's not, add it.
;; Iterate through all frames.
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
	     ;; if the binding of the variable doesn't exist in this frame, add it.
	     (add-binding-to-frame! var val frame))
	    ((eq? var (car vars))
	     (set-car! vals val))
	    (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
	  (frame-values frame))))
	    
	     
;; EVALUATION

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
;; The choice of returning 'ok' is arbitrary, and implementation-dependant.
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
	((variable? exp) (lookup-variable-value exp env))
	((quoted? exp) (text-of-quotation exp))
	((assignment? exp) (eval-assignment exp env))
	((definition? exp) (eval-definition exp env))
	((if? exp) (eval-if exp env))
	((lambda? exp)
	 (make-procedure (lambda-parameters exp)
			 (lambda-body exp)
			 env))
	((begin? exp)
	 (eval-sequence (begin-actions exp) env))
	((cond? exp) (eval (cond->if exp) env))     ; convert the 'cond' into 'if' expressions, and then evaluate.
	((application? exp)
	 (apply (eval (operator exp) env)
		(list-of-values (operands exp) env)))
	(else
	 (error "Unknown expression type -- EVAL" exp))))

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


(define (setup-environment)
  ;; create the initial env extended from the empty one.
  (let ((initial-env
	 (extend-environment (primitive-procedure-names)
			     (primitive-procedure-objects)
			     the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))


;; Primitive procedres.
;; These are connected to the real ones in Lisp.

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

;; The list of actual primitive procedures.
(define primitive-procedures
	 (list (list 'car car)
	       (list 'cdr cdr)
	       (list 'cons cons)
	       (list 'null? null?)
	       (list 'list list)
	       (list 'append append)))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

;; Apply primitive procedures by using the underlying Lisp system.
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

;; Driver loop

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

;; Special procedure to avoid printing the environment as part of a compound procedure
(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
		     (procedure-parameters object)
		     (procedure-body object)
		     '<procedure-env>))
      (display object)))

;; Run the evaluator

(define the-global-environment (setup-environment))

;; Then we would do
;; => (driver-loop) 
