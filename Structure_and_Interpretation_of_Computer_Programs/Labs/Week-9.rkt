#lang simply-scheme

;3.12
(define (set-cdr! x y)
#|   (set! (cdr x) y))

(define (set-car! x y)
  (set! (car x) y))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define x (list 'a 'b)) ; x: (a b)
(define y (list 'c 'd)) ; x: (a b), y: (c d)
(define z (append x y)) ; x: (a b), y: (c d), z: (a b c d)
z
;(a b c d)
(cdr x)
;(b)
(define w (append! x y))
w
;(a b c d)
(cdr x)
;(a b c d); kind of a given since append! returns x. But it just tacks on y to the last element in x
;WHOOPS, (cdr x), not x. so the answer is (b c d)

 |#

 ;2 that would result in trying to set 3 as 2. WRONG Syntatic answer 1: set! requires a symbol.
 ; Semantic answer: set! has nothing to do with pointers

 ;3a
 #| > (define list1 (list (list ’a) ’b))
list1
> (define list2 (list (list ’x) ’y))
list2
> (set-cdr! (car list2) (cdr list1))
okay
> (set-cdr! (car list1) (car list1)))
okay
> list1
((a x b) b)
> list2
((x b) y) |#

;3b check

;3.13
;Instead of pointing to a null, the cdr of the pair that has c as the car point to the pair with
; a as the car. Since no pairs have a null. Last pair will go for ever.

;3.14
;mystery reverses the order of a list. w would give - WRONG! I was going to say (c b a) but then I ran
; the code and got only (a) which upon further reflection becuase the only assignment operation on v
; is the set-cdr! operation that changes v from (a b d) to (a c)