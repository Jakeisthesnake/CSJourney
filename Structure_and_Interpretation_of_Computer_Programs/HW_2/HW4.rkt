#lang simply-scheme

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
  (make-interval (min p1 p2 p3 p4)
                 (max p1 p2 p3 p4))))




;2.7
(define (make-interval a b) (cons a b))
(define (upper-bound interval)
  (max (car interval) (cdr interval)))
(define (lower-bound interval)
  (min (car interval) (cdr interval)))

;2.8
; Given intervals A and B, the interval of subtracting A from B would be:
;upper bound: subtracting the lower bound of A from the upper bound of B
;lower bound: subtracting the upper bound of A from the lower bound of B

 (define (sub-interval x y)
  (make-interval (- (upper-bound x) (lower-bound y))
                 (- (lower-bound x) (upper-bound y))))



(define i1 (make-interval 1 -2))
(define i2 (make-interval 3 4))
(sub-interval i1 i2)
(add-interval i1 i2)

;2.10
(define (div-interval x y)
  (if (and (<= (lower-bound y) 0) (>= (upper-bound y) 0))
      (error  'Devide_by_0)
      (mul-interval
                 x
                 (make-interval (/ 1.0 (upper-bound y))
                                (/ 1.0 (lower-bound y))))))

(div-interval i1 i2)

;2.12
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-center-width c (* c (/ p 100))))
(define (percent i)
  (* 100 (/ (width i) (center i))))
(define i3 (make-center-percent 20 10))
i3
(percent i3)

;2.17
(define (last-pair lis)
  (if (equal? (cdr lis) '())
      (list (car lis))
      (last-pair (cdr lis))))
(last-pair (list 23 72 149 34))

;2.20
(define (same-parity . lis)
  lis
  (define parity-a (remainder (car lis) 2))
  (define (same-parity-a lis)
    (cond ((empty? lis)
           '())
          ((equal? parity-a (remainder (car lis) 2))
           (append (list (car lis)) (same-parity-a (cdr lis))))
          (else (same-parity-a (cdr lis)))))
  (same-parity-a lis))
(same-parity 2 1 2 3 4 5 6 7)
(same-parity 1 1 2 3 4 5 6 7)

;2.22
;The first attempt mixe up the order of the cons by placing the list of quared items in the second
; position which builds the list backwards

;The second attempt flips the order, but by putting the list of squared items first, the answer is a
;list of 2 items made of the list of previoulsly squared terms and the last squared. This creates
;nested lists, rather than a list of all the squared elements.

;2.23
(define (for-each prod lis)
  (if (empty? (cdr lis))
      (prod (car lis))
      ((lambda () (prod (car lis))
                 (for-each prod (cdr lis))))))
(for-each (lambda (x)
                  (newline)
                  (display x))
          (list 57 321 88))
(newline)
;2)
(define (substitute list old new)
  (define (sub-word wd)
    (if (equal? wd old)
         new
         wd))
  (cond ((empty? list)
         '())
        ((list? (car list))
         (cons (substitute (car list) old new)
               (substitute (cdr list) old new)))
        ((word? (car list))
         (cons (sub-word (car list))
               (substitute (cdr list) old new)))
        (else (error 'Not_a_word_or_list))))
(substitute '((lead guitar) (bass guitar) (rhythm guitar) drums)
'guitar 'axe)

;3)
(define (substitute2 list old new)
  (define (match-lists wd list-in list-switch)
    (if (equal? wd (car list-in))
        (car list-switch)
        (match-lists wd (cdr list-in) (cdr list-switch))))
  (define (sub-words wd)
    (if (member? wd old)
         (match-lists wd old new)
         wd))
  (cond ((empty? list)
         '())
        ((list? (car list))
         (cons (substitute2 (car list) old new)
               (substitute2 (cdr list) old new)))
        ((word? (car list))
         (cons (sub-words (car list))
               (substitute2 (cdr list) old new)))
        (else (error 'Not_a_word_or_list))))
(substitute2 '((4 calling birds) (3 french hens) (2 turtle doves))
'(1 2 3 4) '(one two three four))