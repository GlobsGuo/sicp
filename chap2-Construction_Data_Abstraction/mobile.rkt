#lang sicp
(define (make-mobile left right)
  (list left right))

(define (make-branch len structure)
  (list len structure))

(define (left-branch m)
  (car m))
(define (right-branch m)
  (car (cdr m)))

(define (branch-length b)
  (car b))
(define (branch-structure b)
  (car (cdr b)))

(define b1 (make-branch 3 4))
(define b2 (make-branch 4 7))
(define m2 (make-mobile b2 b1))
(display "m2:")
m2
(define b3 (make-branch 5 m2))

(display "m1:")
(make-mobile b3 b2)

(branch-structure b3)

(pair? (right-branch (list 3 4)))

(define (total-weight m)
  (cond ((null? m) 0)
		((not (pair? m)) m)
		(else (+ (total-weight (branch-structure (left-branch m)))
				 (total-weight (branch-structure (right-branch m)))))))

(total-weight m2)
