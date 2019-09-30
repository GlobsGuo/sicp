#lang sicp
;ex-2.7
(define (make-interval a b) (cons a b))

(define (upper-bound c)
  (max (car c) (cdr c)))

(define (lower-bound c)
  (min (car c) (cdr c)))

(define c (make-interval 8 3))
(upper-bound c)
(lower-bound c)

;ex-2.8
(define (sub-interval c1 c2)
  (make-interval (- (lower-bound c1) (upper-bound c2))
				 (- (upper-bound c1) (lower-bound c2))))

(sub-interval c c)

;ex-2.11
;(define (generate-intervals)
;  (define test-list '())
;  (define test-data 
;	(cons (list 0 1 2)
;		  (list -1 -2 -4)))
;  (for-each
;	(lambda (x) (set! test-list (append test-list x)))
;	(map (lambda (x) (map (lambda (y) (make-interval x y))
;						  (cdr test-data)))
;		 (car test-data)))
;  (cons test-list (reverse test-list)))
;(define test-intervals (generate-intervals))
;(display test-intervals)


;ex-2.12
(define (make-center-percent c p)
  (make-interval (* c (- 1 p)) (* c (+ 1 p))))

(define i (make-center-percent 4 0.1))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent i)
  (let ((c (center i)))
	(/ (- (upper-bound i) c) c)))

(percent i)
