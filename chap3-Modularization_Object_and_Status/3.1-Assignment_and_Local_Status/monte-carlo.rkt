#lang racket
(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
  (= (gcd (random) (random)) 1))


(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
	(cond ((= trials-remaining 0)
		   (/ trials-passed trials))
		  ((experiment)
		   (iter (- trials-remaining 1) (+ trials-passed 1)))
		  (else
			(iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))


(define (estimate-integral predicate x1 x2 y1 y2 trials)
  (define (test)
	(let ((x (random))
		  (y (random)))
	  (predicate x y)))
  (* 4.0 (monte-carlo trials test)))

(define (P x y)
  (not (> (+ (* x x) (* y y)) 1)))

(estimate-integral P -1 1 -1 1 1000000)
