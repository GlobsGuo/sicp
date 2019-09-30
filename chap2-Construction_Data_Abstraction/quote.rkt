#lang racket
(list 'a 'b 'c)
(list (list 'george))
(cdr '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))
(pair? (car '(a short list)))

(memq 'red '((red shoes) (blue socks)))
(memq 'red '(red shoes blue socks))
(car (quote(quote(abcds))))
(car ''abracdsdf)

(define (equal-list? l1 l2)
  (cond ((null? l1) (if (null? l2) 
					  #t
					  #f))
		((pair? l1) (if (pair? l2) 
					  (and (equal-list? (car l1) (car l2)) 
					   (equal-list? (cdr l1) (cdr l2)))
					  #f)) 
		(else (if (pair? l2)
				#f
				(eq? l1 l2)))))

(equal-list? '(this is a list) 
			 '(this (is a) list))

