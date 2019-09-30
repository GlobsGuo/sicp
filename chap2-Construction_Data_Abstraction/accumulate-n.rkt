#lang sicp
(define (accumulate op init seq)
  (if (null? seq)
	init
	(op (car seq)
		(accumulate op init (cdr seq)))))

; get the first element of every list to 
; form a new list 
(define seqs (list (list 1 2 3)
				   (list 4 5 6)
				   (list 7 8 9)
				   (list 10 11 12)))
seqs
(define (filter1 s)
  (accumulate cons '() (map
						 (lambda (x)
						   (car x)) 
						 s)))
(filter1 seqs)
(map car seqs)

; get the remain seqences 
(define (remain1 s)
  (accumulate cons '() (map 
						 (lambda (x)
						   (cdr x))
						 s)))
(remain1 seqs)

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
	'()
	(cons (accumulate op init (accumulate cons '() (map
													 (lambda (x)
													   (car x))
													 seqs)))
		  (accumulate-n op init (accumulate cons '() (map
													   (lambda (x)
														 (cdr x))
													   seqs))))))

(accumulate-n + 0 seqs)
