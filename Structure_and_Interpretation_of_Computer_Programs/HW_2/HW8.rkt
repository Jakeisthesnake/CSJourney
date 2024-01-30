#lang simply-scheme




;3.3, 3.4. 3.7
;ok, so there would be a simpler way by just passing the old account to the new account.

(define (make-account balance password)
  (let ((repeats 0))
    (define (withdraw amount)
      (set! repeats 0)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! repeats 0)
      (set! balance (+ balance amount))
      balance)
    (define (call_the_cops)
      (display "Calling the cops"))
    (define (verify)
      #t)
    (define (wrong . tmp)
      (set! repeats (+ repeats 1))
      (if (>= repeats 7)
          (begin
            (call_the_cops)
            #f)
          (begin
            (display "Wrong password. Attempts remaining: ")
            (display (- 7 repeats))
            (newline)
            #f)))
    (define (dispatch password-entry m)
      (cond ((equal? password password-entry)
             (cond ((eq? m 'withdraw) withdraw)
                   ((eq? m 'deposit) deposit)
                   ((eq? m 'verify) verify)
                   (else (error "Unknown request: MAKE-ACCOUNT" m))))
            (else wrong)))
  dispatch))

(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'secret-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'withdraw) 40)
((acc 'secret-password 'deposit) 50)

(define (make-joint account orig-password other-password)
  (if ((account orig-password 'verify))
  (let ((repeats 0))
    (define (withdraw amount)
      ((account orig-password 'withdraw) amount))
    (define (deposit amount)
      ((account orig-password 'deposit) amount))
    (define (call_the_cops)
      "Calling the cops")
    (define (wrong tmp)
      (set! repeats (+ repeats 1))
      (if (>= repeats 7)
          (call_the_cops)
          (begin
            (display "Wrong password. Attempts remaining: ")
            (display (- 7 repeats))
            (newline))))
    (define (dispatch password-entry m)
      (cond ((equal? other-password password-entry)
             (cond ((eq? m 'withdraw) withdraw)
                   ((eq? m 'deposit) deposit)
                   (else (error "Unknown request: MAKE-ACCOUNT" m))))
            (else wrong)))
  dispatch)
  (display "wrong account password")))

(define fake-new-acc (make-joint acc 'some-other-password 'new-secret-password))
(define new-acc (make-joint acc 'secret-password 'new-secret-password))
((new-acc 'new-secret-password 'withdraw) 40)
((new-acc 'some-other-password 'withdraw) 40)
((new-acc 'new-secret-password 'deposit) 40)
((new-acc 'some-other-password 'deposit) 40)
((acc 'secret-password 'withdraw) 40)
((acc 'secret-password 'verify))
((acc 'not-secret-password 'verify))
((acc 'secret-password 'deposit) 50)

;3.8
(define f
  (let ((value 1))
    (lambda (x)
      (if (= 0 x)
          (begin
            (set! value 0)
            value)
          value))))

(define g ;same function rewritten inorder to allow testing both cases at the same time
  (let ((value 1))
    (lambda (x)
      (if (= 0 x)
          (begin
            (set! value 0)
            value)
          value))))


(+ (f 0) (f 1))
(+ (g 1) (g 0))

;3.10
;I drew the structure on paper but got lazy and didn't want to add it here
;There is a second layer in the structure that ecapsulates the evaluation of the let



;, 3.11
;Same as 3.11 had to rewatch the lectures but got it right. Lexical scope - the frame extends
;the frame the lambda was defined in; dynamic scope, the frame extends the frame that calls the lambda

