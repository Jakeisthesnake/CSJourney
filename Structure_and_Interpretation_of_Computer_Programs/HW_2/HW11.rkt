;3.50
(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))


;3.51
(define x
  (stream-map show
              (stream-enumerate-interval 0 10))) ; shows the first element and then returns (0 . promise)
(stream-ref x 5); 5
(stream-ref x 7); 7
;oh wow, after running it I got
"0" ;it shows this when first defined
"1" ;interestingly, no carriagereturn here, since define doesn't return anything
"2"
"3"
"4"
"5" '5 ;then it evaluates the stream until here, showing each element before returning the 5th element

"6"
"7" '7 ;then it evaluates the stream until here, showing each element before returning the 7th element




3.52
(define sum 0) ;sum = 0
(define (accum x)
  (set! sum (+ x sum))
  sum) ;sum = 0
(define seq
  (stream-map accum
              (stream-enumerate-interval 1 20))) ;sum = 1 since only the first element evaluated
(define y (stream-filter even? seq)) ;sum = 6 only first element of y is evaluated and 1 is odd so 2 is the first element of y so seq is evaualted to 2 WRONG sum is 6 since y is the even of seq not the enumeration
(define z
  (stream-filter (lambda (x) (= (remainder x 5) 0))
                 seq));sum = 10 only first element of z is evaluated and 1,3,6 aren't devisible by 5 so 10 is the first element of z so seq is evaualted to 4
(stream-ref y 7) ;sum = 136 only first 8 elements of y is evaluated (6,10,28,36,66,78,120, 136) so seq is evaualted to 16


(display-stream z) ;sum = 210 all of z is shown which evaluates all of seq is evaluated
;3.53
;powers of 2

;3.54
(define (add-streams s1 s2) (stream-map + s1 s2))
(define (mul-streams s1 s2) (stream-map * s1 s2))
(define s (cons-stream 1 (add-streams s s)))
(define ones (cons-stream 1 ones))
(define ints (cons-stream 1 (add-streams ints ones)))

(define factorials
  (cons-stream 1 (mul-streams factorials (add-streams ones ints))))


;3.55
(define (partial-sums strm)
  (define (partial-sums-helper strm a_i)
    (cons-stream (+ a_i (stream-car strm)) (partial-sums-helper (stream-cdr strm) (+ a_i (stream-car strm)))))
  (partial-sums-helper strm 0))



;3.56
(define S (cons-stream 1 (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5)))))


;3.64
(define (stream-limit strm tol)
  (if (< (abs (- (stream-car strm) (stream-car (stream-cdr strm)))) tol)
      (stream-car (stream-cdr strm))
      (stream-limit (stream-cdr strm) tol)))

;3.66
;s = 1 2 3 4 5 ...
;t = a b c d e ...
;1a 2a 2b 3a 3b 4a 3c 5a 4b 6a 4c 7a 5b 8a 4d 9a 6b 10a 
; (i, j) occurs around the (i-1)*2^j spot

;3.68 ;whoop completely wrong. Answer from solution: since interleave is an ordinary procedure,
;(pairs (stream-cdr s) (stream-cdr t)) is evaulated before interleve which calls another interleave
;and thus loops infinitely
; but if the proper delays WERE inplemented, it would lood like this:
;s = 1 2 3 4 5 ...
;t = a b c d e ...
;1a 2b 2a 3c 3a 3b 4a 4d 5a 4b 6a 4c 7a 5b 8a 4d 9a 6b 10a 
; (i, j) occurs exactly at the (i)*2^j - 2^(j-1) spot


;2
(define (fract-stream num den)
  (cons-stream (floor (/ (* 10 num) den))
               (fract-stream (- (* 10 num) (* (floor (/ (* 10 num) den)) den)) den)))


(define (approximation fract-strm n)
  (show-stream fract-strm n))





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

(define (show-stream strm n)
  (if (= n 0)
      '()
      (cons (stream-car strm) (show-stream (stream-cdr strm) (- n 1)))))

(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))
 

(define (exp a b)
    (if (= 0 b)
        1
        (* a (exp a (- b 1)))))

(define a (stream-map + (num-seq 1) (num-seq 2) (num-seq 3) (num-seq 4) (num-seq 5)))



(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (display-line x) (newline) (display x))

(define (show x)
  (display-line x)
  x)

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

;(define x 
;  (stream-map show
;              (stream-enumerate-interval 0 10)))
;(stream-ref x 5)
;(stream-ref x 7)

(define sum 0) ;sum = 0
(define (accum x)
  (set! sum (+ x sum))
  sum) ;sum = 0
(define seq
  (stream-map accum
              (stream-enumerate-interval 1 20))) ;sum = 1 since only the first element evaluated
;(define y (stream-filter even? seq)) ;sum = 6 only first element of y is evaluated and 1 is odd so 2 is the first element of y so seq is evaualted to 2 WRONG sum is 6 since y is the even of seq not the enumeration
;(define z
;  (stream-filter (lambda (x) (= (remainder x 5) 0))
;                 seq));sum = 10 only first element of z is evaluated and 1,3,6 aren't devisible by 5 so 10 is the first element of z so seq is evaualted to 4
;(stream-ref y 7) ;sum = 136 only first 8 elements of y is evaluated (6,10,28,36,66,78,120, 136) so seq is evaualted to 16
;
;
;(display-stream z) ;sum = 210 all of z is shown which evaluates all of seq is evaluated

(define (add-streams s1 s2) (stream-map + s1 s2))
(define (mul-streams s1 s2) (stream-map * s1 s2))
(define s (cons-stream 1 (add-streams s s)))
(define ones (cons-stream 1 ones))
(define ints (cons-stream 1 (add-streams ints ones)))

(define factorials
  (cons-stream 1 (mul-streams factorials (add-streams ones ints))))

(define (partial-sums strm)
  (define (partial-sums-helper strm a_i)
    (cons-stream (+ a_i (stream-car strm)) (partial-sums-helper (stream-cdr strm) (+ a_i (stream-car strm)))))
  (partial-sums-helper strm 0))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream
                   s2car
                   (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1)
                          (stream-cdr s2)))))))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define S (cons-stream 1 (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5)))))

(define (stream-limit strm tol)
  (if (< (abs (- (stream-car strm) (stream-car (stream-cdr strm)))) tol)
      (stream-car (stream-cdr strm))
      (stream-limit (stream-cdr strm) tol)))

(define (sqrt-improve guess x)
  (/ (+ guess (/ x guess)) 2))

(define (sqrt-stream x)
  (define guesses
    (cons-stream
     1.0
     (stream-map (lambda (guess) (sqrt-improve guess x))
                 guesses)))
  guesses)

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))
