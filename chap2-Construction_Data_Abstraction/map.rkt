#lang sicp
(define (square x) (* x x))
(define (square-list1 items)
  (if (null? items)
	'()
	(cons (square (car items)) (square-list1 (cdr items)))))

(square-list1 (list 1 2 3 4))

(define (square-list2 items)
  (map square items))

(square-list2 (list 2 3 4 5))

;for-each
(define (for-each1 f l)
  (cond
	((null? l) #t)
	(else (f (car l)) 
		  (for-each1 f (cdr l)))))

(for-each1 
  (lambda (x) (display (square x)) (newline))
  (list 1 2 3 4))
