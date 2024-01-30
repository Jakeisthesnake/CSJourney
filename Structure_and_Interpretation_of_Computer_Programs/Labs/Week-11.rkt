#sicp

;1 a: a procedure. Answer: close, a promise
;1b: 28

;2 (stream-cdr (stream-cdr (cons-stream 1 ’(2 3))))
; (force (cdr (force (cdr (1 . (delay '(2 3)))))))
; (force (cdr (force (delay '(2 3)))))
; (force (cdr '(2 3)))
; (force '(3))
; not a procedure? although I would think it might simply return the list.
;ah force expects a promise.

;3
;(delay (enumerate-interval 1 3)) will give the entire interval if it is forced
;(stream-enumerate-interval 1 3) will give each next inteval number with subsequent forces

;4
#lang sicp
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (num-seq n)
  (cond ((even? n)
         (cons-stream n (num-seq (/ n 2))))
        ((odd? n)
         (cons-stream n (num-seq (+ (* n 3) 1))))
        (else 'hmmmm)))

(define (return-stream strm)
  (if (= (stream-car strm) 1)
      (list 1 4 2 1 '...)
      (cons (stream-car strm) (return-stream (stream-cdr strm)))))

(define (seq-length strm)
  (if (= (stream-car strm) 1)
      1
      (+ 1 (seq-length (stream-cdr strm)))))