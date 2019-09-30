#lang sicp
(define l1 (list 1 3 (list 5 7) 9))
(car (cdr (car (cddr l1))))

(define l2 (list (list 7)))
(car (car l2))

(define l3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l3))))))))))))

(define x (list 1 2 3 4))
(define y (list 3 4 5 6))
(append x (list 8 9))
(cons x y)
(define tree (list x y))
(define t2 (list (list x) (list y)))
t2

(define (reverse-list l)
  (define (iter l r)
	(if (null? l)
	  r
	  (iter (cdr l) (cons (car l) r))))
  (iter l '()))

(reverse-list tree)

(define l4 (cons 1 nil))
(define l5 (cons 2 l4))
(define l6 (cons 3 l5))
l4
l5
l6

(pair? l5)
(pair? l4)

(define (reverse-recur l)
  (if (null? l)
	'()
	(append (reverse-recur (cdr l)) (list (car l)))))
(reverse-recur x)

(define (reverse-tree t)
  (cond ((null? t) '())
		((pair? (car t))
		 (append 
		   (reverse-tree (cdr t))
		   (list (reverse-tree (car t)))))
		(else (append 
				(reverse-tree (cdr t))
				(list (car t))))))

(reverse-tree tree)

(define (fringe t)
  (define (helper l r)
	(cond ((null? l) r)
		  ((pair? l) (helper (car l) (helper (cdr l) r)))
		  (else (cons l r))))
  (helper t '()))
(fringe tree)
