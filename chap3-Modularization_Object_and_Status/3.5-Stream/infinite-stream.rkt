#lang racket
(require sicp)
(define (debug prefix . params) 
  (display prefix) 
  (newline)
  (for-each (lambda (x) (display x) (newline)) params))


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

(define (mul-streams s1 s2)
  (stream-maps * s1 s2))
(define (add-streams s1 s2)
  (stream-maps + s1 s2))
(define (scale-stream s x)
  (stream-map (lambda (e) (* x e)) s))

; ex-3.50
(define (stream-maps proc . argstreams)
  (if (stream-null? (car argstreams))
	the-empty-stream
	(cons-stream
	  (apply proc (map stream-car argstreams))
	  (apply stream-maps
			 (cons proc (map stream-cdr argstreams))))))


(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))
(stream-ref integers 4)


(define factorials
  (cons-stream 1 (mul-streams factorials 
							  (stream-cdr integers))))

(define (partial-sums s)
  (cons-stream (stream-car s)
			   (add-streams (partial-sums s)
							(stream-cdr s))))


(define (stream-for-each proc s)
  (if (stream-null? s)
	'done
	(begin (proc (stream-car s))
		   (stream-for-each proc (stream-cdr s)))))
(define (show-stream s)
  (stream-for-each (lambda (x)
					 (display x) (newline))
				   s))

(stream-ref (partial-sums integers) 5)


(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
		((stream-null? s2) s1)
		(else
		  (let ((s1car (stream-car s1))
				(s2car (stream-car s2)))
			(cond ((< s1car s2car)
				   (cons-stream s1car (merge (stream-cdr s1) s2)))
				  ((> s1car s2car)
				   (cons-stream s2car (merge s1 (stream-cdr s2))))
				  (else
					(cons-stream s1car (merge (stream-cdr s1)
											  (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (merge (scale-stream S 2)
									   (scale-stream S 3))
								(scale-stream S 5))))

(define (integrate-series s)
	(stream-maps / s integers)) 

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(define cosine-series
  (cons-stream 1 (integrate-series (scale-stream sine-series -1))))

(stream-ref sine-series 5)

