#lang sicp
(define (accumulate op initial sequence)
  (if (null? sequence)
	initial
	(op (car sequence)
		(accumulate op initial (cdr sequence)))))

(define fold-right1 accumulate)

(define (fold-left1 op initial sequence)
  (define (iter result rest)
	(if (null? rest)
	  result 
	  (iter (op result (car rest))
			(cdr rest))))
  (iter initial sequence))


(define (reverse1 sequence)
  (fold-left1 (lambda (x y) (cons y x)) '() sequence))

(reverse1 (list 1 2 3))

(define (reverse2 sequence)
  (fold-right1 (lambda (x y) 
				 (append y (list x))) '() sequence))
(reverse2 (list 1 2 3))
