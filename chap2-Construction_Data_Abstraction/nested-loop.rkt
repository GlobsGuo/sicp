#lang racket
(define (accumulate op initial sequence)
  (if (null? sequence)
	initial
	(op (car sequence)
		(accumulate op initial (cdr sequence)))))

(define (enumerate-interval a b)
  (if (= a b)
	'()
	(cons a (enumerate-interval (+ a 1) b))))

(define (unique-pairs n)
  (accumulate append '() 
			  (map (lambda (i)
					 (map (lambda (j) (list i j))
						  (enumerate-interval 1 i)))
				   (enumerate-interval 2 n))))

(unique-pairs 7)

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime? n)
  (define (divides? a b) (= (remainder b a) 0))
  (define (find-divisor num test-divisor)
	(cond ((> ((lambda (x) (* x x)) test-divisor) num) num)
		  ((divides? test-divisor num) test-divisor)
		  (else (find-divisor num (+ test-divisor 1)))))
  (define (smallest-divisor num)
	(find-divisor n 2))
  (= n (smallest-divisor n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
	   (filter prime-sum? 
			(unique-pairs n))))

(prime-sum-pairs 7)


(define (sum-3p p)
  (+ (car p) (cadr p) (caddr p)))

(define (enumerate n)
  (define (helper start end)
	(if (> start end) '()
	  (cons start (helper (+ start 1) end))))
  (helper 1 n))

(define (enumerate-diff x n)
  (define (helper start end x) 
	(cond ((> start end) '())
		  ((= start x) (helper (+ start 1) end x))
		  (else (cons start (helper (+ start 1) end x)))))
  (helper 1 n x))


(define (enumerate-diff2 x y n)
  (define (helper start end x y)
	(cond ((> start end) '())
		  ((= start x) (helper (+ start 1) end x y))
		  ((= start y) (helper (+ start 1) end x y))
		  (else (cons start (helper (+ start 1) end x y)))))
  (helper 1 n x y))

(define (make-3p n)
  (accumulate append '()
			  (accumulate append '() 
						  (map (lambda (i) 
								 (map (lambda (j)
										(map (lambda (k) (list i j k))
											 (enumerate-diff2 i j n)))
									  (enumerate-diff i n)))
							   (enumerate n)))))

(define (make-3p-sum n sum)
  (filter (lambda (p) (= (sum-3p p) sum)) 
		  (make-3p n)))

(make-3p-sum 6 9)
