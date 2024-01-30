#lang simply-scheme

;Ex 2
(display "\n")
(define (switch p ow nw) ;phrase old-word new-word
  (cond ((empty? p)
          '())
        ((equal? (first p) ow)
         (se nw (switch (bf p) ow nw)))
        (#t
         (se (first p) (switch (bf p) ow nw)))))
(switch '(she loves you yeah yeah yeah) 'yeah 'maybe)

;Ex 3
(display "\n")
;g is a function of 0 args and returns a function
;i.e.
(define (g)
  (lambda (x) (+ x 2)))
((g) 1)

;Ex 4
(display "\n")

(display "\n")
(define f1 1)
f1 ; a variable

(display "\n")
(define (f2) (display 1))
(f2) ; a function of 0 args

(display "\n")
(define (f3 x) (display x))
(f3 3) ; a funtion that takes 1 numerical arg

(display "\n")
(define (f4)
  (lambda () (display 1)))
((f4)) ; a definition of a function that takes 0 args


(display "\n")
(define (f5)
  (lambda ()
    (lambda (x) (display x))))
(((f5)) 3) ; a definition of a funtion that takes 1 numerical arg


; Ex 5
(display "\n")
;guess a: 3
;guess b: 9
;guess c: 27
(define (t f)
  (lambda (x) (f (f (f x)))) )
(define (1+ x) (+ x 1))
((t 1+) 0) ;correct
((t (t 1+)) 0) ;correct
(((t t) 1+) 0) ;correct

; Ex 6
;Same as problem 5

; Ex 7
(display "\n")
(define (make-tester wd)
  (lambda (x)
    (if (equal? wd x)
        #t
        #f)))
((make-tester 'hal) 'hal)
((make-tester 'hal) 'cs61a)
(define sicp-author-and-astronomer? (make-tester 'gerry))
(sicp-author-and-astronomer? 'hal)
(sicp-author-and-astronomer? 'gerry)