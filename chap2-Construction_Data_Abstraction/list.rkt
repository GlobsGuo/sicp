#lang sicp
(define (list-n l n)
  (if (= n 0)
	(car l)
	(list-n (cdr l) (- n 1))))
(define (list-length l)
  (if (null? l)
	0
	(+ (list-length (cdr l)) 1)))
(define (last-pair l)
  (list (list-n l (- (list-length l) 1))))

(define l (list 1 2 3 4 5))
(list-length l)
(list-n l 3)
(last-pair l)

(define (list-reverse l)
  (define (helper l r)
	(if (null? l)
	  r
	  (helper (cdr l) (cons (car l) r))))
  (helper l '()))

(list-reverse l)

(define (same-parity . l)
  (define (helper l r)
	(if (= (list-length l) 0)
	  (reverse r)
	  (if (= (remainder (- (car l) (car r)) 2) 0)
		(helper (cdr l) (cons (car l) r))
		(helper (cdr l) r))))
  (helper (cdr l) (list (car l))))

(same-parity 2 3 4 5 66 8 9)
