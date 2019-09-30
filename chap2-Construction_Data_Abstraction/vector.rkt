#lang racket
(define (make-vec x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(ycor-vect (make-vec 3 4))

(define (sub-vect v1 v2)
  (make-vec (- (xcor-vect v1)
			   (xcor-vect v2))
			(- (ycor-vect v1)
			   (ycor-vect v2))))

(define (add-vect v1 v2)
  (make-vec (+ (xcor-vect v1)
			   (xcor-vect v2))
			(+ (ycor-vect v1)
			   (ycor-vect v2))))

(define (scale-vect v s)
  (make-vec (* s (xcor-vect v))
			(* s (ycor-vect v))))
