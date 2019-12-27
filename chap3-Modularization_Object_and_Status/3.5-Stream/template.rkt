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



