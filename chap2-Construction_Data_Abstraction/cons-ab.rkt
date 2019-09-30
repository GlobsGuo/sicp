#lang sicp
(define plus4 (lambda (x) (+ x 4)))
(define (cons-ab a b)
  (define (xexpn x n)
  	(if (= n 0) 
	  1
	  (* x (xexpn x (- n 1)))))
  (* (xexpn 2 a)
	 (xexpn 3 b)))
(define c (cons-ab 4 5))
(display c)
(newline)

(define (car-ab c)
  (define (car-helper a c)
	(if (= (remainder c 2) 1)
	  a
	  (car-helper (+ a 1) (/ c 2))))
  (car-helper 0 c))

(car-ab c)

(define (cdr-ab c)
  (define (cdr-helper b c)
	(if (> (remainder c 3) 0)
	  b
	  (cdr-helper (+ b 1) (/ c 3))))
  (cdr-helper 0 c))

(cdr-ab c)
