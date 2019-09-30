#lang sicp
(define (accumulate op initial sequence)
  (if (null? sequence)
	initial
	(op (car sequence)
		(accumulate op initial (cdr sequence)))))

(define (accumulate-n op initial seqs)
  (if (null? (car seqs))
	'()
	(cons (accumulate op initial (map car seqs))
		  (accumulate-n op initial (map cdr seqs)))))

(define seqs (list (list 1 2 3)
				   (list 4 5 6)
				   (list 7 8 9)
				   (list 10 11 12)))
(accumulate-n + 0 seqs)

(define vec (list 2 3 4))
(define (matrix-*-vector m v)
  (map m))
