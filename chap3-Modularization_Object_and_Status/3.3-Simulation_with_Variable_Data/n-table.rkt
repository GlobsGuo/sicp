#lang racket 
(require sicp)
(define (debug x) (display x) (newline))

(define (make-table same-key?)
  (let ((table (list '*table*)))
	; find a given key in table
	(define (find-key k t)
	  (cond ((null? t) false)
			((same-key? k (caar t)) (car t))
			(else (find-key k (cdr t)))))
	; lookup a value with given keys-table
	(define (lookup keys-table)
	  (define (helper keys-table local-table)
		(debug local-table)
		(let ((key (car keys-table)))
		  (let ((record (find-key key local-table)))
			(debug record)
			(debug keys-table)
			(if record 
			  (if (pair? (cdr keys-table))
				(helper (cdr keys-table) (cdr local-table))
				(cdr record))
			  false))))
	  (helper keys-table (cdr table)))
	; insert a value given by keys-table
	(define (insert! keys-table value)
	  (define (helper keys-table local-table)
		(let ((key (car keys-table)))
		  (let ((record (find-key key (cdr local-table))))
			(debug key)
			(debug record)
			(debug local-table)
			(if record
			  (if (pair? (cdr keys-table))
				(helper (cdr keys-table) (cdr local-table))
				(set-cdr! record value)) ; update value
			  (begin   
				  ; if the key doesn't exist, we need to add
				  ; one. Here we know that the key doesn't 
				  ; exist is the last one in key-tables.
				  ; Because we don't have the valuea of all
				  ; the remaining keys.
				  (debug (cdr local-table))
				  (set-cdr! local-table
							(cons 
							  (cons key value)
							  (cdr local-table))))))))
	  (helper keys-table table))
	; dispatch
	(define (dispatch m)
	  (cond ((eq? m 'lookup) lookup)
			((eq? m 'insert!) insert!)
			(else (debug "Unknown operation"))))
	dispatch))

(define t1 (make-table eq?))
((t1 'lookup) '(a b))

(t1 'sdf)
((t1 'insert!) '(a) '3)
(debug "second insert!")
((t1 'insert!) '(a b) '4)
((t1 'lookup) '(a))
((t1 'insert!) '(a b c) '6)
((t1 'insert!) '(a b d) '8)
((t1 'lookup) '(a b c))
