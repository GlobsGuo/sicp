#lang racket
; entry of a tree
(define (entry tree) (car tree))

; tree's left branch
(define (left-branch tree) (cadr tree))

; tree's right branch
(define (right-branch tree) (caddr tree))

; make a tree using entry and two branches
(define (make-tree entry left right)
  (list entry left right))

; element-of-set? based on tree
(define (element-of-set? x set)
  (cond ((= x (entry set)) true)
		((< x (entry set))
		 (element-of-set? x (left-branch set)))
		(else 
		  (element-of-set? x (right-branch set)))))

; adjoin-set based on tree
(define (adjoin-set x set)
  (cond ((null? set)
		 (make-tree x '() '()))
		((= x (entry set)) set)
		((< x (entry set))
		 (make-tree (entry set)
					(adjoin-set x (left-branch set))
					(right-branch set)))
		((> x (entry set))
		 (make-tree (entry set)
					(left-branch set)
					(adjoin-set x (right-branch set))))))

; parse a tree into a list
(define (tree->list-1 tree)
  (if (null? tree)
	'()
	(append (tree->list-1 (left-branch tree))
			(cons (entry tree)
				  (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
	(if (null? tree)
	  result-list
	  (copy-to-list (left-branch tree)
					(cons (entry tree)
						  (copy-to-list (right-branch tree))))))
  (copy-to-list tree '()))

(define (partial-tree elts n)
  (if (= n 0)
	(cons '() elts)
	(let ((left-size (quotient (- n 1) 2)))
	  (let ((left-result (partial-tree elts left-size)))
		(let ((left-tree (car left-result))
			  (non-left-elts (cdr left-result))
			  (right-size (- n (+ left-size 1))))
		  (let ((this-entry (car non-left-elts))
				(right-result (partial-tree (cdr non-left-elts)
											right-size)))
			(let ((right-tree (car right-result))
				  (remaining-elts (cdr right-result)))
			  (cons (make-tree this-entry left-tree right-tree)
					remaining-elts))))))))
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define s2 (list 1 3 5 7 9 11))
(define s1 (list 1 2 3 4 5 6))
(define t1(list->tree s1))
(define t2(list->tree s2))

; union based on bi-tree
; our union doesn't keep the bi-tree structure.
(define (union set1 set2)
  (define (helper s1 s2)
	(cond ((null? s1) s2)
		  ((null? s2) s1)
		  (else 
			(let ((x1 (entry s1))
				  (x2 (entry s2)))
			  (cond ((= x1 x2) 
					 (append (list x1)
							 (helper (left-branch s1) (left-branch s2)) 
							 (helper (right-branch s1) (right-branch s2))))
					((< x1 x2) 
					 (append (helper 
							   (make-tree x1 (left-branch s1) '())
							   (left-branch s2)) 
							 (helper
							   (right-branch s1)
							   (make-tree x2 '() (right-branch s2)))))
					((> x1 x2) 
					 (append (helper 
							   (left-branch s1)
							   (make-tree x2 (left-branch s2) '()))
							 (helper 
							   (make-tree x1 '() (right-branch s1))
							   (right-branch s2)))))))))
  (helper set1 set2))

(define (accumulate op init seq)
  (if (null? seq)
	init
	(op (car seq) 
		(accumulate op init (cdr seq)))))
(define (strip-null seq)
  (accumulate cons
			  '() 
			  seq))

; intersect based on bi-tree
(define (intersection-set set1 set2)
  (define (helper s1 s2)
	(display s1)
	(newline)
	(display s2)
	(newline)
	(newline)
	(if (or (null? s1) (null? s2))
	  '()
	  (let ((x1 (entry s1))
			(x2 (entry s2)))
		(cond ((= x1 x2)
			   (append (list x1)
					   (helper (left-branch s1) (left-branch s2))
					   (helper (right-branch s1) (right-branch s2))))
			  ((> x1 x2)
			   (append (helper (left-branch s1) 
							   (make-tree x2 (left-branch s2) '()))
					   (helper (make-tree x1 '() (right-branch s1))
							   (right-branch s2))))
			  ((< x1 x2)
			   (append (helper (make-tree x1 (left-branch s1) '())
							   (left-branch s2))
					   (helper (right-branch s1)
							   (make-tree x2 '() (right-branch s2)))))))))
  (helper set1 set2))

(display "intersection t1 and t2:")
(intersection-set t1 t2)

