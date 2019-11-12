#lang racket
; determine if x belongs to set
(define (element-of-set? x set)
  (cond ((null? set) false)
		((equal? x (car set)) true)
		(else (element-of-set? x (cdr set)))))
; ordered element-of-set?
(define (element-of-set-ordered? x set)
  (cond ((= x (car set)) true)
		((> x (car set)) false)
		(else (< x (car set)) 
			  (element-of-set-ordered? x (cdr set)))))

; add x to set
(define (adjoin-set x set)
  (if (element-of-set? x set)
	set
	(cons x set)))
; ordered adjoin
(define (adjoin-set-ordered x set)
  (cond ((= x (car set)) set)
		((< x (car set))
		 (cons x set))
		(else (cons (car set) 
					(adjoin-set-ordered x (cdr set))))))

; intersect set1 and set2
(define (intersection-set set1 set2)
  (cond ((null? set1) '())
		((null? set2) '())
		((element-of-set? (car set1) set2)
		 (cons (car set1) (intersection-set (cdr set1) set2)))
		(else (intersection-set (cdr set1) set2))))
; ordered intersect
(define (intersection-set-ordered set1 set2)
  (cond ((null? set1) '())
		((null? set2) '())
		(else (let ((x1 (car set1))
					(x2 (car set2)))
				(cond ((= x1 x2)
					   (cons x1 
							 (intersection-set-ordered (cdr set1)
													   (cdr set2))))
					   ((< x1 x2)
						(intersection-set-ordered (cdr set1) set2))
					   (else (intersection-set-ordered set1 (cdr set2))))))))

; union set1 and set2
(define (union-set set1 set2)
  (cond ((null? set1) set2)
		((null? set2) set1)
		((element-of-set? (car set1) set2)
		 (union-set (cdr set1) set2))
		(else (cons (car set1) (union-set (cdr set1) set2)))))
; ordered union
(define (union-set-ordered set1 set2)
  (cond ((null? set1) set2)
		((null? set2) set1)
		(else (let ((x1 (car set1))
					(x2 (car set2)))
				(cond ((= x1 x2) 
					   (union-set-ordered (cdr set1) set2))
					  ((> x1 x2)
					   (cons x2 
							 (union-set-ordered set1 (cdr set2))))
					  (else
						(cons x1
							  (union-set-ordered (cdr set1) set2))))))))

(define s1 (list 1 2 3 4 5))
(define s2 (list 1 3 5 7 9))
(intersection-set s1 s2)
(adjoin-set-ordered 4 s2)
(intersection-set-ordered s1 s2)

(union-set s1 s2)
(union-set-ordered s1 s2)
