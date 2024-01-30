#lang simply-scheme

;1 a) change cond statements of denominations in dictionary
; b) the program be rewritten to iterate up to the total kinds of coins


;2) You gain a factor of efficincy equal to the highest demonimation devided by the lowest
; if you cut of branches starting with the highest first. Actually i thinkj it is highest devided by lowest
; plus (lowest devide by highest times second highest dvided by lowest) plus (simplifide third highest by highest)

(define (count-change amount) (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((or (< amount 0) (= kinds-of-coins 0)) 0)
        ((= amount 0) 1)
        (else (+ (cc amount (- kinds-of-coins 1))
                 (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 2) 1)
        ((= kinds-of-coins 1) 5)))
        ;((= kinds-of-coins 3) 10)
        ;((= kinds-of-coins 2) 25)
        ;((= kinds-of-coins 1) 50)))
(trace cc)
(cc 5 2)

;ok, it is definitely less than a what I thought. But I might not be running big enough numbers.

; Anyways the reason is that starting with larger demoninations prunes off branches much more quickly.
;I.e. 100 - 75 - 50 - 25 with quarters instead of every number with pennies.

;3
#| (define (count-change amount) (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (empty? kinds-of-coins)) 0)
        (else (+ (cc amount (bf kinds-of-coins))
                 (cc (- amount (first kinds-of-coins)) kinds-of-coins)))))
(cc 600 '(50 25 10 5 1)) |#

;4
(define (square x) (* x x))
(define (type-check f test? input)
  (if (test? input)
      (f input)
      #f))
(type-check square number? 'hello)
(type-check square number? 4)

;5
(define (make-safe f test?)
  (define (g input)
    (if (test? input)
      (f input)
      #f))
  g)
(define safe-square (make-safe square number?))

(safe-square 'hello)
(safe-square 4)