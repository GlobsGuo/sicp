#lang racket
(define (debug x)
  (display x)
  (newline))

(define (make-accumulator base)
  (lambda (x) (begin (set! base (+ base x))
		 base)))

(define (make-monitored f)
  (let ((count 0))
	(define (dispatch m)
	  (cond ((eq? m 'how-many-calls?) count)
			((eq? m 'reset-count) (set! count 0))
			(else (begin (set! count (+ count 1))
						 (f m)))))
	dispatch))

(define mon (make-monitored sqrt))
(mon 100)
(mon 64)
(mon 'how-many-calls?)
(debug "monitored done")
  

