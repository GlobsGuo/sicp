#lang racket 
(require sicp)
(define (debug prefix . params) 
  (display prefix) 
  (newline)
  (for-each (lambda (x) (display x) (newline)) params))

(define (for-each-except exception proc l)
  (define (loop items)
	(cond ((null? items) 'done)
		  ((eq? (car items) exception)
		   (loop (cdr items)))
		  (else (proc (car items))
				(loop (cdr items))))
	(loop l)))
(define (make-connector)
  (let ((value #f) (infor #f) (constraints '()))
	(define (set-my-value newval setter)
	  (cond ((not (has-value? me))
			 (begin (set! value newval)
					(set! infor setter)
					(for-each-except setter
									 inform-about-value
									 constraints))) ; end begin
			((not (= value newval))
			 (debug "Contradiction" (list value newval)))
			(else 'ignored))) ; end function set-my-value
	(define (forget-my-value retractor)
	  (if (eq? retractor infor)
		(begin (set! infor #f)
			   (for-each-except retractor
								inform-about-no-value
								constraints)) ; end begin
		'ignored)) ; end function forget-my-value
	(define (connect new-constraint)
	  (if (not (memq new-constraint constraints))
		(set! constraints 
		  (cons new-constraint constraints))
		(if (has-value? me)
		  (inform-about-value new-constraint))
		'done)) ;end function connect
	(define (me request)
	  (cond ((eq? request 'has-value?)
			 (if infor true false))
			((eq? request 'value) value)
			((eq? request 'set-value!) set-my-value)
			((eq? request 'forget) forget-my-value)
			((eq? request 'connect) connect)
			(else (debug "Unknown operation -- CONNECTOR" request))))
	me))

; syntactic sugar
(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value infor)
  ((connector 'set-value!) new-value infor))
(define (forget-value! connector restractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

;; constraint adder
(define (adder a1 a2 sum)
  (define (process-new-value)
	(cond ((and (has-value? a1) (has-value? a2))
		   (set-value! sum
					   (+ (get-value a1) (get-value a2))
					   me))
		  ((and (has-value? a1) (has-value? sum))
		   (set-value! a2
					   (- (get-value sum) (get-value a1))
					   me))
		  ((and (has-value a2) (has-value? sum))
		   (set-value! a1 
					   (- (get-value sum) (get-value a2))
					   me)))) ; end function process-new-value
  (define (process-forget-value)
	(forget-value! sum me)
	(forget-value! a1 me)
	(forget-value! a2 me)
	(process-new-value)) ; end function process-forget-value
  (define (me request)
	(cond ((eq? request 'I-have-a-value)
		   (process-new-value))
		  ((eq? request 'I-lost-my-value)
		   (process-forget-value))
		  (else
			(debug "Unknown request -- ADDER" request)))) ; end function me
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me) ; end constraint adder
;; constraint multiplier
(define (multiplier m1 m2 product)
  (define (process-new-value)
	(cond ((or (and (has-value? m1) (= (get-value m1) 0))
			   (and (has-value? m2) (= (get-value m2) 0)))
		   (set-value! product 0 me))
		  ((and (has-value? m1) (has-value? m2))
		   (set-value! product
					   (* (get-value m1) (get-value m2))
					   me))
		  ((and (has-value? product) (has-value? m1))
		   (set-value! m2 
					   (/ (get-value product) (get-value m1))
					   me))
		  ((and (has-value? product) (has-value? m2))
		   (set-value! m1 
					   (/ (get-value product) (get-value m2))
					   me)))) ; end function process-new-value
  (define (process-forget-value)
	(forget-value! product me)
	(forget-value! m1 me)
	(forget-value! m2 me)
	(process-new-value)) ; end function process-forget-value
  (define (me request)
	(cond ((eq? request 'I-have-a-value)
		   (process-new-value))
		  ((eq? request 'I-lost-my-value)
		   (process-forget-value))
		  (else
			(debug "Unknown request -- MULTIPLIER" request)))) ; end function me
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me) ; end constrait multiplier

;; constarint constant
(define (constant value connector)
  (define (me request)
	(error "Uknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)
(define (inform-about-value constraint)
  (constraint 'I-hav-a-value))
(define (inform-about-no-value constraint)
  (constarint 'I-lost-my-value))


(define (probe name connector)
  (define (print-probe value)
	(newline)
	(display "Probe: ")
	(display name)
	(display " = ")
	(display value))
  (define (process-new-value)
	(print-probe (get-value connector)))
  (define (process-forget-value)
	(print-probe "?"))
  (define (me request)
	(cond ((eq? request 'I-have-a-value)
		   (process-new-value))
		  ((eq? request 'I-lost-my-value)
		   (process-forget-value))
		  (else
			(debug "Unknown request -- PROBE" request))))
  (connect connector me)
  me)

(define (averanger a b c)
  (let ((sum make-connector) 
		(divisor make-connector))
	(adder a b sum)
	(constant 2 divisor)
	(multiplier divisor c sum)
	'ok))

(define (squarer a b)
  (define (process-new-value)
	(if (has-value? b)
	  (if (< (get-value b) 0)
		(error "square less than 0 -- SQUARER" (get-value b))
		(cond ((has-value? a)
			   (set-value! b (* (get-value a)
								(get-value a))
						   me))
			  ((has-value? b)
			   (set-value! a (sqrt (get-value b)) 
						   me))))))
  (define (process-forget-value)
	(forget-value! a me)
	(forget-value! b me)
	(process-new-value))
  (define (me request)
	(cond ((eq? request 'I-have-a-new-value)
		   (process-new-value))
		  ((eq? request 'I-lost-my-value)
		   (process-forget-value))
		  (else
			(debug "Unknown request -- SQUARER" request))))
  (connect a me)
  (connect b me)
  me)


(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
		  x)
	  (cv 32)))

(define (c+ x y)
  (let ((z (make-connector)))
	(adder x y z)
	z))

(define (c* x y)
  (let ((z (make-connector)))
	(multiplier x y z)
	z))

(define (c/ x y)
  (let ((z (make-connector)))
	(multiplier z y x)
	z))
(define (cv x)
  (let ((z (make-connector)))
	(constant x z)
	z))
