#lang sicp

(define (make-rat n d)
  (let ((g (gcd n d)))
  	(if (< (* n d) 0) 
	  (cons (- (abs (/ n g))) (abs (/ d g)))
	  (cons (abs (/ n g)) (abs (/ d g))))))

(numer (make-rat 8 10))
(numer (make-rat -30 90))

