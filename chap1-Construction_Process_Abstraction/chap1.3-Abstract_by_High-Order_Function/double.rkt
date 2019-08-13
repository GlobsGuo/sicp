#!/usr/bin/racket
#lang sicp 

(define (double f)
  (lambda (x)
	(f (f x))))

(define (inc x)
  (+ 1 x))

((double inc) 1)

((double ((double double) inc)) 5)


(define (compose f g)
  (lambda (x)
	(f (g x))))

(define (square x)
  (* x x))

((compose square inc) 6)

(define (repeated f n)
	(if (= n 1) f
	  (compose f (repeated f (- n 1)))))

((repeated square 2) 5)


(define (smooth g)
  (define dx 0.00001)
  (lambda (x)
	(/ (+ (g (- x dx)) (g x) (g (+ x dx))) 3)))

(define (n-smooth f n)
  ((repeated (smooth f)) n))
