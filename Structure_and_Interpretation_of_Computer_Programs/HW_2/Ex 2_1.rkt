#lang simply-scheme

;Ex 1.31(a)
(display '1.31a)
(display "\n")
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))
;test 1
(define (by1 x) x)
(define (next1 x) (+ 1 x))
(product by1 2 next1 4)

(define (factorial n) (product by1 1 next1 n))

;test
(factorial 4)

(define (pi-term n)
  (if (even? n)
      (/ (+ n 2) (+ n 1))
      (/ (+ n 1) (+ n 2))))
(define (pi-4 n) (product pi-term 1.0 next1 n))
;test
 (* (pi-4 10000) 4)

;Ex 1.32(a)
(display "\n")
(display '1.32a)
(display "\n")
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
         (accumulate combiner null-value term (next a) next b))))
;sum
(define (sum a b)(accumulate + 0 by1 a next1 b))
(sum 1 4)
;product
(define (multiply a b)(accumulate * 1 by1 1 next1 4))
(multiply 1 4)

;Ex 1.33
(display "\n")
(display '1.33)
(display "\n")
(define (accumulate-filter pred combiner null-value term a next b)
  ;(display a)
  (cond ((> a b)
         null-value)
        ((pred a b)
         (combiner (term a) (accumulate-filter pred combiner null-value term (next a) next b)))
        (else (combiner null-value (accumulate-filter pred combiner null-value term (next a) next b)))))
;1.33a
(display "\n")
(display '1.33a)
(display "\n")
(define (square x) (* x x))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n blank)
  (= n (smallest-divisor n)))

(define (prime-squares a b)(accumulate-filter prime? + 0 square a next1 b))

(prime-squares 1 5)
;1.33b
(display "\n")
(display '1.33b)
(display "\n")
(define (product-coprimes n)(accumulate-filter coprime? * 1 by1 1 next1 n))

(define (gcd a b)
   (if (< a b)
      (gcd b a)
      (if (= b 0)
          a
          (gcd b (remainder a b)))))

(define (coprime? a b)
  (equal? (gcd a b) 1))
(product-coprimes 8)


;1.40
(display "\n")
(display 'Ex1.40)
(display "\n")

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

(define dx 0.00001)
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))
;actual answer next two lines
(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

;test
;(newtons-method (cubic 2 -3 -1) 0)

;1.41
(display "\n")
(display 'Ex1.41)
(display "\n")
(define (double f)
  (lambda (x) (f (f x))))
(define (inc x) (+ x 1))
(((double (double double)) inc) 5)


;1.43
(display "\n")
(display 'Ex1.43)
(display "\n")

(define (compose f g)
  (lambda (x) (f (g x))))

(define (compose-n f g n)
  (define (iter-compose g n)
    (if (= n 1)
        (lambda (x) (g x))
        (iter-compose (compose f g) (- n 1))))
  (iter-compose f n))

(define (repeated f n)
  (compose-n f f n))
((repeated square 2) 5)


;1.46
(display "\n")
(display 'Ex1.46)
(display "\n")
;actual answer
(define (iterative-improve-guess good-enough? improve-guess guess)
  (if (good-enough? guess)
      guess
      (iterative-improve-guess good-enough? improve-guess (improve-guess guess))))

(define (iterative-improve good-enough? improve-guess)
  (lambda (guess) (iterative-improve-guess good-enough? improve-guess guess)))

(display "\n")
(display 'square)
(display "\n")

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (define (good-enough-square? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve-guess-square guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough-square? improve-guess-square) 1.0))
(sqrt 5)

(display "\n")
(display 'fixed-point)
(display "\n")


(define (fixed-point-146 f x)
  (define tolerance 0.00001)
  (define (good-enough-fixed? guess)
    (< (abs (- guess (f guess))) tolerance))
  (define (improve-guess-fixed guess)
    (f guess))
  ((iterative-improve good-enough-fixed? improve-guess-fixed) 1.0))
(fixed-point-146 cos 1)

(display "\n")
(display 'Ex2)
(display "\n")

(define (every f phrase)
  (if (empty? phrase)
      '()
      (sentence (f (first phrase)) (every f (bf phrase)))))
(every square '(1 2 3 4))
(every first '(nowhere man))

(display "\n")
(display 'Ex3)
(display "\n")
(every (lambda (letter) (word letter letter)) 'purple)
(every (lambda (number) (if (even? number) (word number number) number))
'(781 5 76 909 24))
(keep even? '(781 5 76 909 24))
(keep (lambda (letter) (member? letter 'aeiou)) 'bookkeeper)
(keep (lambda (letter) (member? letter 'aeiou)) 'syzygy)
;(keep (lambda (letter) (member? letter 'qaeiou)) '(purple syzygy))
;(keep (lambda (wd) (member? 'e wd)) '(purple syzygy))

;bonus - didn't figure it out, looked it up recreating answer to really let it sink in

#| (define (fact n)
  (if (equal? n 0)
      1
      (* n (fact (- n 1)))))
(fact 4) |#

#| ((lambda (x) (x x)
  (lambda (n) ))) |#
  