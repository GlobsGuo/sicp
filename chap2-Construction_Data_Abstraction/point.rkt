#lang sicp
;point definition
(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))
;point test
(define p (make-point 3 2))
(print-point p)
(define q (make-point 5 6))

;line definition
(define (make-segment p1 p2)
  (cons p1 p2))
(define (start-segment l)
  (car l))
(define (end-segment l)
  (cdr l))
;line test
(define line (make-segment p q))
(print-point (start-segment line))

(define (midpoint-segment l)
  (make-point 
	(/ (+ (car (start-segment l)) (car (end-segment l))) 2)
  	(/ (+ (cdr (start-segment l)) (cdr (end-segment l))) 2)))

(print-point (midpoint-segment line))

;rectangle definition
(define (rectangle l1 l2)
  (cons l1 l2))
(define (distance p1 p2)
  (define (square x) (* x x))
  (sqrt (+ (square 
			 (- (x-point p1) (x-point p2)))
		   (square
			 (- (y-point p1) (y-point p2)))
		   )))
(define (len r)
  (distance (start-segment (car r)) (end-segment (car r))))
(define (wid r)
  (distance (start-segment (car r)) (start-segment (cdr r))))

(define (area r)
  (* (len r) (wid r)))
(define (circum r)
  (* 2 (+ (len r) (wid r))))

(define r1 (rectangle line line))
(area r1)
(circum r1)
