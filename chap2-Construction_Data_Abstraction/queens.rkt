#lang racket
(define (accumulate op initial sequence)
  (if (null? sequence) 
	initial
	(op (car sequence)
		(accumulate op initial (cdr sequence)))))

(define (flat-map proc seq)
  (accumulate append '() (map proc seq)))

(define (enumerate-interval start end)
  (if (> start end)
	'()
	(cons start (enumerate-interval (+ start 1) end))))

(enumerate-interval 2 6)

(define (board colum row)
  (list (list colum row)))

(define (adjoin-position new-row col board)
  (append board (list (list col new-row))))

(define empty-board '())

; We have all the rows and columns when we 
; filter board using safe?
; So, given a column number, we can get the 
; corresponding row value from board.
(define (get-nth n board)
  (define (helper i n board)
	(if (= i n) 
	  (car (cdr (car board)))
	  (helper (+ i 1) n (cdr board))))
  (helper 1 n board))

(define (safe? col board)
  (let ((r (get-nth col board)))
	(define (helper i col board)
	  (cond ((= i col) #t)
			((= r (get-nth i board)) #f)
			((= (abs (- r (get-nth i board))) 
				(abs (- i col))) #f)
			(else (helper (+ i 1) col board))))
	(helper 1 col board)))

(define (queens board-size)
  (define (queen-cols k)
	(if (= k 0)
	  (list empty-board)
	  (filter
		(lambda (positions) (safe? k positions))
		(flat-map
		  (lambda (rest-of-queens)
			(map (lambda (new-row)
				   (adjoin-position new-row k rest-of-queens))
				 (enumerate-interval 1 board-size)))
		  (queen-cols (- k 1))))))
  (queen-cols board-size))

(length (queens 8))
