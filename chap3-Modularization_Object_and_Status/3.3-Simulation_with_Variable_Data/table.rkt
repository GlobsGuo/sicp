#lang racket 
(require sicp)
(define (debug x) (display x) (newline))

(define (make-table same-key?)
  (let ((table (list '*table*)))
	(define (find-key k t)
	  (cond ((null? t) false)
			((same-key? k (caar t)) (car t))
			(else (find-key k (cdr t)))))
	(define (lookup key)
	  (let ((record (find-key key (cdr table))))
		(if record 
		  (cdr record)
		  false)))
	(define (insert! key value)
	  (let ((record (find-key key (cdr table))))
		(debug record)
		(if record
		  (set-cdr! record value)
		  (set-cdr! table 
					(cons 
					  (cons key value) 
					  (cdr table))))))
	(define (dispatch m)
	  (cond ((eq? m 'lookup) lookup)
			((eq? m 'insert!) insert!)
			(else (debug "Unknown operation"))))
	dispatch))

(define t1 (make-table eq?))
((t1 'lookup) 'A)

(t1 'sdf)
((t1 'insert!) 'A '4)
((t1 'lookup) 'A)
((t1 'insert!) 'B '3)
((t1 'lookup) 'B)
