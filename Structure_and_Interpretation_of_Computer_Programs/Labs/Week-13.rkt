#lang simply-scheme

;1
(define play-line-counts
  (mapreduce (lambda (input-key-value-pair)
                (list (make-kv-pair (car input-key-value-pair) 1))) ; mapper
             + ; reducer
             0 ; base case
              "/gutenberg/shakespeare"))

  (mapreduce (lambda (input-key-value-pair)
                (list (make-kv-pair 'play-counts (cdr input-key-value-pair)))) ; mapper
             + ; reducer
             0 ; base case
              play-line-counts)

;2)
(define (sentence-to-words input-key-value-pair)
  (define (helper input)
    (if (empty? input)
        '()
        (cons (cons (car input 1)) (helper (cdr input)))) )
  (helper (cdr input-key-value-pair)))

(define word-counts
  (mapreduce sentence-to-words ; mapper
             + ; reducer
             0 ; base case
              "/gutenberg/shakespeare"))

(define (stream-max stream)
  (define (helper stream current-max max-key)
    (cond ((empty? stream)
           max-key)
          ((> (cdr (stream-car stream)) current-max)
           (helper (stream-cdr stream) (cdr (stream-car stream)) (car (stream-car stream))))
          (else
           (helper (stream-cdr stream) current-max max-key ))))
  (helper stream 0 '()))

(define (word-once? input-key-value-pair)
  (if (= (cdr input-key-value-pair) 1)
      #t
      #f))

  (stream-filter word-once? stream)

;3a
(mapreduce match-map ; mapper
             match-reduce ; reducer
             the-empty-stream ; base case
              "/gutenberg/shakespeare"))


(define (match-map pattern)
  (lambda (input-key-value-pair)
    (if (match? (cdr input-key-value-pair) pattern)
        (cons 1 (input-key-value-pair))
        (cons #f '()))))

(define (match-reduce sofar new)
  (if (equal? '() new)
      sofar
      (stream-cons new sofar)))