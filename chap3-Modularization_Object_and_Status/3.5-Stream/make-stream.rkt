#lang racket 
(require sicp)
(define (debug prefix . params) 
  (display prefix) 
  (newline)
  (for-each (lambda (x) (display x) (newline)) params))

;(define (memo-proc proc)
;  (let ((already-run? #f) (result #f))
;	(lambda ()
;	  (if (not already-run?)
;		(begin (set! result (proc))
;			   (set! already-run? #t)
;			   result)
;		result))))
;
;(define (delay-proc proc)
;  (lambda () proc))
;  ;(memo-proc (lambda () proc)))
(define (force-proc delayed-object)
  (delayed-object))


;(define (cons-stream a proc)
;  (cons a (delay-proc proc)))
;(define-syntax cons-stream
;  (syntax-rules ()
;				[(cons-stream x y) (cons x (delay-proc y))]))

;; We use sicp's cons-stream impletion, which is 
;; defined in ~/.racket/7.5/pkgs/sicp/sicp/main.rkt.
;; The sicp use r5rs:cons and delay to define cons-stream,
;; and the delayed proc need to be called by force.
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

;(define the-empty-stream '())
;(define stream-null? null?)

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

; ex-3.50
(define (stream-maps proc . argstreams)
  (if (stream-null? (car argstreams))
	the-empty-stream
	(cons-stream
	  (apply proc (map stream-car argstreams))
	  (apply stream-maps
			 (cons proc (map stream-cdr argstreams))))))

(define (stream-for-each proc s)
  (if (stream-null? s)
	'done
	(begin (proc (stream-car s))
		   (stream-for-each proc (stream-cdr s)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
	the-empty-stream
	(cons-stream 
	  low
	  (stream-enumerate-interval (+ low 1) high))))

(define (display-stream s)
  (stream-for-each (lambda (x) (display x) (newline)) s))

(define stream (stream-enumerate-interval 0 10))

(define (display-line x) (display x) (newline))

(define (show x)
  (display-line x)
  x)

;(stream-map show stream)
(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
		((pred (stream-car stream))
		 (cons-stream (stream-car stream)
					  (stream-filter pred
									 (stream-car stream))))
		(else (stream-filter pred (stream-cdr stream)))))

(define (add-streams s1 s2)
  (stream-maps + s1 s2))


