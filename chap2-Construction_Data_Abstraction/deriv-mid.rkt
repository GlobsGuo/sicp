#lang racket
(define (deriv expr var)
  (cond ((number? expr) 0)
		((variable? expr)
		 (if (same-variable? expr var) 1 0))
		((sum? expr)
		 (make-sum (deriv (addend expr) var)
				   (deriv (augend expr) var)))
		((product? expr)
		 (make-sum (make-product (deriv (multiplier expr) var)
								 (multiplicand expr))
				   (make-product (deriv (multiplicand expr) var)
								 (multiplier expr))))
		(else
		  (display "unknown expression"))))

(define (accumulate op init seq)
  (if (null? seq)
	init
	(op (car seq) 
		(accumulate op init (cdr seq)))))


(define (=number? x n)
  (and (number? x) (= x n)))
(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (addend s) (car s))
(define (augend s) 
  (if (sum? (cddr s))
	(cddr s)
	(caddr s)))
;(define (augend s) (caddr s))

(define (multiplier p) (car p))
(define (multiplicand p) 
  (if (product? (cddr p))
	(cddr p)
	(caddr p)))
;(define (multiplicand p) (caddr p))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
		((=number? a2 0) a1)
		((and (number? a1) (number? a2)) (+ a1 a2))
		(else (list 'a1 + a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
		((=number? m1 1) m2)
		((=number? m2 1) m1)
		((and (number? m1) (number? m2)) (* m1 m2))
		(else (list 'm1 * m2))))


;(deriv '(x * y) 'x)
;(deriv '(x * 3) 'x)

;(deriv '(x * (+ x 3)) 'x)



