#lang sicp
; 1.29
(define (cube x) (* x x x))
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (sum term a next b)
    (if (> a b)
        0
        (+ (term a) (sum term (next a) next b))))
  ; 1.30
  (define (sum-iter term a next b)
    (define (iter a result)
      (if (> a b)
          result
          (iter (next a) (+ (term a) result))))
    (iter a 0))
  (define (y k)
    (f(+ a (* k h))))
  (define (term k)
    (cond ((or (= k 0) (= k n)) (y k))
          ((even? k) (* 2 (y k)))
          (else (* 4 (y k)))))
  (* (sum-iter term 0 inc n) (/ h 3)))

