#lang simply-scheme

;4.27
c = 2 from the def of w (if the def of id, the set! is not evaluated)
w = 10 (and will always = 10)
c = 2 still because w was memoized and only the result was saved, not the process.
Wrong: interesting -> c = 1 then c = 2 because defining w only makes a promise to evalate the inner
id so only the outer id is evaluated making c = 1. when w is called, the promise is evaluated and the
inner id is called, making c = 2.

;4.29
;a program that doesn't reuse variables much or where variables are changed with each iteration...
(define (bad-memo x a)
  (if (= x 0)
      'done
      (begin
        (set! a x)
        (bad-memo (- x 1) a))))

;with memos
sq id -> c = 1

without memos
sq id -> c = 2


