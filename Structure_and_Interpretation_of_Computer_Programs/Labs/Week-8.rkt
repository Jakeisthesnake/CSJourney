#lang simply-scheme


(define (make-account init-amount)
  (let ((balance init-amount)(transaction-list '()))
    (define (withdraw amount)
      (set! transaction-list (append transaction-list (list (list 'withdraw amount))))
      (set! balance (- balance amount))
      balance)
    (define (show-balance)
      balance)
    (define (show-init-amount)
      init-amount)
    (define (deposit amount)
      (set! transaction-list (append transaction-list (list (list 'deposit amount))))
      (set! balance (+ balance amount))
      balance)
    (define (show-transactions)
      transaction-list)
    (define (dispatch msg)
      (cond
        ((eq? msg 'withdraw) withdraw)
        ((eq? msg 'deposit) deposit)
        ((eq? msg 'balance) show-balance)
        ((eq? msg 'init-balance) show-init-amount)
        ((eq? msg 'transactions) show-transactions) ) )
    dispatch) )

(define acc (make-account 100))
((acc 'withdraw) 50)
((acc 'withdraw) 30)
((acc 'deposit) 40)
((acc 'withdraw) 60)
((acc 'init-balance))
((acc 'balance))
((acc 'transactions))


(define (plus1 var)
  (set! var (+ var 1))
  var)

(plus1 5)
;((lambda (var) (set! var (+ var 1)) var) 5)
;(set! 5 (+ 5 1)) 5
;Error cant change constant?
;WRONG actual result: 6 NOT WRONG? Solution says what I said...?

(define (make-adder n) ;returns the procedure that adds n to its input
  (lambda (x) (+ x n)))

(make-adder 3); returns the procedure that adds three to its input

((make-adder 3) 5); 8

(define (f x) (make-adder 3)); makes f the procedure that adds three to its input but requires an extra input WRONG defines f as the make adder function which correctly takes a single input

(f 5); returns the procedure that adds three to its input WRONG 8

(define g (make-adder 3)); makes g the procedure that adds three to its input

(g 5); 8


(define (make-funny-adder n) ;returns the procedure that adds n to its input unless its input is 'new in which case it increments n by 1
  (lambda (x)
    (if (equal? x 'new)
        (set! n (+ n 1))
        (+ x n))))

(define h (make-funny-adder 3)); defines h as the procedure that adds 3 to its input unless its input is 'new in which case it increments 3 by 1 and so forth

(define j (make-funny-adder 7)) ; defines j as the procedure that adds 7 to its input unless its input is 'new in which case it increments 7 by 1 and so forth

(h 5) ; 8

(h 5) ;8

(h 'new) ;6? WRONG returns nothing

(h 5) ; 9

(let ((a 3)) ;error no body? WHOOPS 8 
  (+ 5 a))

(let ((a 3)) ;returns the funtion that add 3 in input
  (lambda (x) (+ x a)))

((let ((a 3)) ; 8
   (lambda (x) (+ x a)))
 5)

((lambda (x) ; evaluates the function that adds three to input with 5 as imput i.e. 8
   (let ((a 3))
     (+ x a)))
 5)

(define k ; defines k as the function that adds three to input 
  (let ((a 3))
    (lambda (x) (+ x a))))

(k 5) ;8

(define m ; defines m as the function that adds three to input 
  (lambda (x)
    (let ((a 3))
      (+ x a))))

(m 5) ;8

(define p ;defines p as the procedure that adds 3 to its input unless its input is 'new in which case it increments 3 by 1
  (let ((a 3))
    (lambda (x)
      (if (equal? x 'new)
          (set! a (+ a 1))
          (+ x a)))))

(p 5) ;8
(p 5) ;8
(p 'new) ;nothing returned
(p 5);9

(define r ;hmmm I think it wants to be like p but since the let is inside the lambda it will just get reset each time?
  (lambda (x)
    (let ((a 3))
      (if (equal? x 'new)
          (set! a (+ a 1))
          (+ x a)))))
r
(r 5) ;8
(r 5) ;8
(r 'new)
(r 5) ;8? Ha! correct!

(define s; defines s as the procedure that returns a procedure that increments a by 1 if s's input is new or the procedure that add 3 to its second input when s gets add as its input
  (let ((a 3))
    (lambda (msg)
      (cond ((equal? msg 'new)
             (lambda ()
               (set! a (+ a 1))))
            ((equal? msg 'add)
             (lambda (x) (+ x a)))
            (else (error "huh?"))))))

(s 'add) ; procedure

;(s 'add 5) ;too many arguments

((s 'add) 5) ;8

(s 'new) ;procedure
((s 'add) 5) ;8
((s 'new)) ; increments a by 1
((s 'add) 5) ;9

(define (ask obj msg . args)
  (apply (obj msg) args))

(ask s 'add 5); 9

(ask s 'new)

(ask s 'add 5) ;10

(define x 5)

(let ((x 10) ;defines f as the function that add 10 to its input
      (f (lambda (y) (+ x y))))
  (f 7)) ;17 WRONG! interesting, i guess since it is in the same let as (x 10) it isn't high enough to call it, so it uses (x 10)?

;(let ((a 10)
;      (f (lambda (y) (+ a y)))) ; ah yes, a is unbound
;  (f 7))

;(define x 5) ;Whoops already defined