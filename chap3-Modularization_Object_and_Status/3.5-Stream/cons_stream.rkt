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


(define (interleave s1 s2)
  (if (stream-nul? s1)
	s2
	(cons-stream (stream-car s1)
				 (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
	(list (stream-car s) (stream-car t))
	(interleave 
	  (stream-map (lambda (x) (list (stream-car s) x))
				  (stream-cdr t))
	  (pairs (stream-cdr s) (stream-cdr t)))))
  
; ex-3.67
(define (pairs-all s t)
  (cons-stream
	(list (stream-car s) (stream-car t))
	(interleave
	  (iterleave (stream-map (lambda (x) (list (stream-car s) x))
							 (stream-cdr t))
				 (pairs (stream-cdr s) (stream-cdr t)))
	  (stream-map (lambda (x) (list (stream-car t) x))
				  (stream-cdr s)))))

; ex-3.69
(define (triples s t u)
  (cons-stream
	(list (stream-car s) (stream-car t) (stream-car u))
	(interleave
	  (stream-map (lambda (x) (cons (stream-car s) x))
				  (stream-cdr (pairs t u)))
	  (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define phythagorean-triples
  (define (square x) (* x x))
  (stream-filter (lambda (t)
				   (= (square (caddr x))
					  (+ (square (car x))
						 (square (cadr x)))))
				 (triples integers integers integers)))

; ex-3.70
(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
		((stream-null? s2) s1)
		(else
		  (let ((s1car (stream-car s1))
				(s2car (stream-car s2)))
			(let ((w1 (weight s1car))
				  (w2 (weight s2car)))
			  (cond ((< s1car s2car)
					 (cons-stream s1car (merge (stream-cdr s1) s2)))
					((> s1car s2car)
					 (cons-stream s2car (merge s1 (stream-cdr s2))))
					(else
					  (cons-stream s1car 
								   (cons-stream s2car 
												(merge (stream-cdr s1)
													   (stream-cdr s2)))))))))))


