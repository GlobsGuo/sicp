#lang sicp
(define (accumulate op initial sequence)
  (if (null? sequence)
	initial
	(op (car sequence)
		(accumulate op initial (cdr sequence)))))

(define (square x) (* x x))

(define (map1 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(map1 square (list 1 2 3))

(define (append1 seq1 seq2)
  (accumulate cons seq1 seq2))

(append (list 1 2 3) (list 4 5 6))

(define (length1 sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(length1 (list 1 2 3 4 5 6))
