;; 3.1.1

;; Here, balance is defined globally, which is not desirable.
(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
	     balance)
      "Insufficient funds"))

;; The variable balance is encapsulated inside new-withdraw.
(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
	  (begin (set! balance (- balance amount))
		 balance)
	  "Insufficient funds"))))

;; This creates "withdrawl processors"
;; The parameter balance functions as a local variable, for each procedure created.
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds")))

;; Simple bank account system
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  (else (error "Unknown request -- MAKE-ACCOUNT"
		       m))))
  dispatch)


;; Exercise 3.1
;; Accumulator
;; Returns accumulator objects which maintain local totals.
(define (make-accumulator total)
  (lambda (arg)
    (begin (set! total (+ total arg))
	   total)))


;; Exercise 3.2
;; Monitor calls
(define (make-monitored f)
  (let ((count 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls?) count)
	    ((eq? x 'reset-count) (set! count 0))
	    (else (begin (set! count (+ 1 count))
			 (f x)))))))
;; Exercise 3.3
;; Simple bank account system
(define (make-account-secure balance password)

  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (incorrect-password x)
    "Incorrect password")

  (define (dispatch p m)
    (let ((account-password password))
      (if (eq? p account-password)
	  (cond ((eq? m 'withdraw) withdraw)
		((eq? m 'deposit) deposit)
		(else
		 (error "Unknown request -- MAKE-ACCOUNT" m)))
	  incorrect-password)))
  dispatch)

