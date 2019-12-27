#lang racket
(require sicp)

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

; stream operation
(define (stream-ref s n)
  (if (= n 0)
	(stream-car s)
	(stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
	the-empty-stream
	(cons-stream (proc (stream-car s))
				 (stream-map proc (stream-cdr s)))))


;(define (sqrt-stream x)
;  (define guesses
;	(cons-stream 1.0
;				 (stream-map (lambda (guess)
;							   (sqrt-improve guess x))
;							 guesses)))
;  guesses)

(define (stream-maps proc . argstreams)
  (if (stream-null? (car argstreams))
	the-empty-stream
	(cons-stream
	  (apply proc (map stream-car argstreams))
	  (apply stream-maps
			 (cons proc (map stream-cdr argstreams))))))


(define (add-streams s1 s2)
  (stream-maps + s1 s2))

(define (partial-sums s)
  (cons-stream (stream-car s)
			   (add-streams (partial-sums s)
							(stream-cdr s))))

(define (scale-stream s x)
  (stream-map (lambda (e) (* x e)) s))


(define (pi-summands n)
  (cons-stream (/ 1.0 n)
			   (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(stream-ref pi-stream 3)


; ex-3.64
(define (stream-limit s delta)
  (let ((s0 (stream-ref s 0))
		(s1 (stream-ref s 1)))
	(if (< (abs (- s0 s1)) delta)
	  s1
	  (stream-limit (stream-cdr s) delta))))

(define (pi tolerance)
  (stream-limit pi-stream tolerance))

(pi 0.01)

; ex-3.65
(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
			   (stream-map - (ln2-summands (+ n 1)))))
(define ln2-stream 
  (partial-sums (ln2-summands 1)))
(stream-ref ln2-stream 3)
