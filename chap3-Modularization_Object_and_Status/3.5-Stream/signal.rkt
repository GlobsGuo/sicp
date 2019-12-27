#lang racket 
(require sicp)
(define (debug prefix . params) 
  (display prefix) 
  (newline)
  (for-each (lambda (x) (display x) (newline)) params))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
(define (stream-map proc s)
  (if (stream-null? s)
	the-empty-stream
	(cons-stream (proc (stream-car s))
				 (stream-map proc (stream-cdr s)))))
(define (stream-maps proc . sargs)
  (if (stream-null? (car sargs))
	the-empty-stream
	(cons-stream 
	  (apply proc (map stream-car sargs))
	  (apply stream-maps
			 (cons proc (map stream-cdr sargs))))))
(define (scale-stream s x)
  (stream-map (lambda (e) (* e x)) s))
(define (add-streams s1 s2)
  (stream-maps + s1 s2))
(define (stream-ref s index)
  (if (= index 0)
	(stream-car s)
	(stream-ref (stream-cdr s) (- index 1))))
(define ones (cons-stream 1 ones))
(define integers
  (cons-stream 1 (add-streams ones integers)))


(define (integral integrand initial-value dt)
  (define int
	(cons-stream initial-value
				 (add-streams (scale-stream integrand dt)
							  int)))
  int)

; ex-3.73
(define (RC R C dt)
  (lambda (i v0)
	(add-streams (integral (scale-stream i (/ 1 C)) v0 dt)
				 (scale-stream i R))))

(stream-ref ones 3)
(stream-ref integers 5)
(stream-ref (integral integers 3 0.0001) 4)

(define RC1 (RC 5 1 0.5))
(stream-ref (RC1 integers 3) 3)

; ex-3.74
;(define zero-crossings
;  (stream-maps sign-change-detector 
;			   sense-data 
;			   (cons-stream 0 zero-crossings)))

(define (smooth s)
  (define (avg a b) (/ (+ a b) 2))
  (define (helper stream last)
	(cons-stream (avg last (stream-car stream))
				 (helper (stream-cdr stream) (stream-car stream))))
  (helper (stream-cdr s) (stream-car s)))

(stream-ref (smooth integers) 3)
(stream-ref (smooth integers) 4)
(stream-ref (smooth integers) 5)

