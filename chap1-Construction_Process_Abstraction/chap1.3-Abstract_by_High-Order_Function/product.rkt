#lang sicp
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (cube x) (* x x x))
(define (calc f a b)
  (product f a inc b))
(define (double-inc x) (inc (inc x)))

(define (numerator x)
  (* (+ x 1) 4 x))

(define (square x) (* x x))
(define (denominator x)
  (square (+ (* 2 x) 1)))

(define (pi n)
  (* 4 (/ (product numerator 1 inc n) (product denominator 1 inc n))))

(define (factorial n)
  (define (this x) (+ x 0))
  (product this 1 inc n))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (pi-iter n)
  (* 4 (/ (product-iter numerator 1 inc n) (product-iter denominator 1 inc n))))