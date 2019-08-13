#!/usr/bin/racket
#lang sicp 
(define delta 0.0000001)
(define (close-enough? v1 v2)
  (< (abs (- v1 v2)) delta))

(define (fixed-point f first-guess)
  (define (try guess)
	(display guess)
	(newline)
	(let ((next (f guess)))
	  (if (close-enough? guess next)
		next
		(try next))))
  (try first-guess))

(define (fixed-point-avg f first-guess)
  (define (try guess)
	(display guess)
	(newline)
	(let ((next (/ (+ (f guess) guess) 2)))
	  (if (close-enough? guess next)
		next
		(try next))))
  (try first-guess))

(define (f x)
  (+ 1 (/ 1 x)))
(fixed-point f 0.5)

(newline)
(fixed-point-avg f 0.5)

(newline)
(define (g x)
  (/ (log 1000) (log x)))
(fixed-point g 2)

(newline)
(fixed-point-avg g 2)
