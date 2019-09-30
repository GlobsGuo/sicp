#lang sicp
(define tree (list 1
				   (list 2 (list 3 4) 5) (list 6 7)))
tree

(define (square x) (* x x))
(define (square-tree1 t)
  (cond ((null? t) '())
		((not (pair? t)) (square t))
		(else (cons (square-tree1 (car t))
					(square-tree1 (cdr t))))))

(square-tree1 tree)

(define (square-tree2 t)
  (map (lambda (tree)
		 (if (pair? tree)
		   (square-tree2 tree)
		   (square tree)))
		 t))

(square-tree2 tree)

(define (tree-map f t)
  (map (lambda (st)
		 (cond ((null? st) '())
			   ((not (pair? st)) (f st))
			   (else (tree-map f st))))
	   t))

(define (square-tree3 t)
  (tree-map square t))

(square-tree3 tree)
