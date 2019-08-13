#!/usr/bin/racket
#lang sicp 
(define delta 0.0001)
(define (close-enough? v1)
  (let ((pusai (/ 2 (+ 1 (sqrt 5)))))
  (< (abs (- v1 pusai)) delta)))

(define (cont-frac n d k)
  (define (loop result term)
	(if (= term 0)
	  result
	  (loop (/ (n term)
			   (+ (d term) result))
			(- term 1))))
  (loop 0 k))

(define (cont-frac-recur n d k)
  (define (frac-recur i)
	(if (> i k)
	  0
	  (/ (n i) (+ (d i) (frac-recur (+ 1 i))))))
  (frac-recur 1))

(define (f k)
  (cont-frac-recur (lambda (i) 1.0)
		   (lambda (i) 1.0)
		   k))

(define (get-k k)
  (display (f k))
  (newline)
  (if (close-enough? (f k))
	k
	(get-k (+ k 1))))

(get-k 0)

(define (euler k)
  (cont-frac-recur (lambda (i) 1.0) 
			 (lambda (i)
    		   (cond ((= (remainder i 3) 2) (* 2 (+ 1 (/ i 3)))) 
					 (else 1)))
			 k))

(euler 1000)


