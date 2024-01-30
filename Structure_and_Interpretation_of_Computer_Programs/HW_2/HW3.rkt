#lang simply-scheme

; 1.16
(display "\n")
(display 'Ex1.16)
(display "\n")
(define (exp-n b n)
  (define (exp-iter a a-exp b-i b-i-exp) 
    (cond ((equal? a-exp n)
           a)
          ((<= (+ a-exp b-i-exp) n)
           (exp-iter (* a b-i) (+ a-exp b-i-exp) (* b-i b-i) (+ b-i-exp b-i-exp)))
          (else
           (exp-iter a a-exp b 1))))
  ;(trace exp-iter)
  (exp-iter 1 0 b 1))

(exp-n 2 11)

;better answer from solutions:
#| (define (exp-n b n)
  (define (exp-iter a b n) 
    (cond ((equal? n 0)
           a)
          ((even? n)
           (exp-iter a (square b) (/ n 2)))
          (else
           (exp-iter (* a b) b (- n 1)))))
  (exp-iter 1 b n)) |#

; 1.35
(display "\n")
(display 'Ex1.35)
(display "\n")

; x = 1 + 1/x
; x^2 = x + 1
; x^2 - x - 1 = 0
; x = (1 +/- sqrt(5))/2
(/ (+ 1 (sqrt 5)) 2)

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


(define (phi x)
  (+ 1 (/ 1 x)))
(fixed-point phi 1.0)

; 1.37
(display "\n")
(display 'Ex1.37)
(display "\n")

(define (cont-frac n d k)
  (define (cont-frac-iter i frac)
    (if (equal? i 1)
      (/ (n 1) (+ (d 1) frac))
      (cont-frac-iter (- i 1) (/ (n i) (+ (d i) frac)) )))
  ;(trace cont-frac-iter)
  (cont-frac-iter k 0.0))
(cont-frac (lambda (i) i) (lambda (i) i) 3)

; k = 13

(define (cont-frac-rec n d k)
  (define (cont-frac-iter-rec i)
    (if (equal? i k)
      (/ (n i) (d i) )
      (/ (n i) (+ (d i) (cont-frac-iter-rec (+ i 1))))))
  ;(trace cont-frac-iter-rec)
  (cont-frac-iter-rec 1.0))
(cont-frac-rec (lambda (i) i) (lambda (i) i) 3)

;1/(1 +2/(2+3/3)) = .6

;1.38
(display "\n")
(display 'Ex1.38)
(display "\n")
(define (n i) i)
(define (d i)
  (if (equal? 0 (remainder (+ i 1) 3))
      (* 2 (/ (+ i 1) 3))
      1))
(cont-frac n d 3)
#| (d 1)
(d 2)
(d 3)
(d 4)
(d 5)
(d 6)
(d 7) |#

;2
(display "\n")
(display '2)
(display "\n")

; could be optimized by obl
(define (factor n)
  (define (factor-iter i phrase)
    (cond ((equal? i n)
           (sentence phrase '()))
          ((equal? (remainder n i) 0)
           (factor-iter (+ i 1) (sentence i phrase)))
          (else (factor-iter (+ i 1) phrase))))
  (factor-iter 1 '()))

(define (sum-of-factor n)
  (define (factor-iter i total)
    (cond ((equal? i n)
           total)
          ((equal? (remainder n i) 0)
           (factor-iter (+ i 1) (+ i total)))
          (else (factor-iter (+ i 1) total))))
  (factor-iter 1 0))

(define (next-perf n)
  (if (equal? (+ 1 n) (sum-of-factor (+ n 1)))
       (+ n 1)
       (next-perf (+ 1 n))))

(factor 496)
(sum-of-factor 496)
(next-perf 6)

;3
; the evaluation order of base cases doesn't matter since it isn't possible for overlap
; the only possibility is (cc a 0) where a <= 0. However, since the alogrithm only ever
;decreases the first or second argument and terminates if either the first or second arg is 0 (or
; below 0 in the case of the first arg) it is impossible to have a case with overlap.

;Ah after reading the solution I see that I forgot if (cc 0 0) was called independantly, i.e. how many
; ways can you count 0 cents with no change, in which case the answer should be 1. So the book's
;order is correct.

;4 b^n = product * b^(counter)

;5a
(define (number-of-partitions n)
  (define (n-array i phrase)
    (if (equal? i 0)
        '()
        (sentence i (n-array (- i 1) phrase))))
  (define (partition n_i i-phrase)
    (cond ((= n_i 0) 1)
          ((or (< n_i 0) (empty? i-phrase)) 0)
          (else (+ (partition n_i (bf i-phrase))
                 (partition (- n_i (first i-phrase)) i-phrase)))))
  (partition n (n-array n '())))
(number-of-partitions 5)

;5b Partitions are like counting coins where n is the amount and the numbers less than and equal to
;n are the kinds of coins

;5c One way would be to generate the n^2*(n+1)/2 possible arrays where the terms are non increasing
; and then run though the list and tally which ones add to n

; but I think the real question is how to keep the flavor of the coin counting algorythm while
; making it iterative
;hmmmm well maybe have a counter to pass the discovered partition and then the last partition?
; i.e. 10 cents with dimes nickles and pennies (10,5,1) gives 1,0,0;  0,2,0;  0,1,5;  and 0,0,10
; the numbers are sorted, so there should be a way of navigating to the point in the tree where
; the search should continue.