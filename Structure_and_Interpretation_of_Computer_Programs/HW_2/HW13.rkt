#lang simply-scheme

;4.22 done in racket
;4.23 since cond only returns the last item, A's code would only execute the last process. Wrong
; I completely missed the point. Both programs execute the code, but the book's version does it
; much more efficiently since it only passes the functions, not the execution code of A which has
;conditionals that will be executed everytime.

;4.24
;old 24 second for (define a (factorial 100000))
;new 18 second for (define a (factorial 100000))
;c + cycles-a*(pause_a + a) + cycles-e*(pause_e + e)
;c + cycles-a*(a) + cycles-e*(pause_e + e)
;c + cycles-a*(pause_a + a) + cycles-e*(e)
;c + cycles-a*(a) + cycles-e*(e)
;could remove c with an different duration pause

;1a
(define (invert-index directory n)
  (define (word-length-less-than-n? word n)
    (define (word-length-iter rest total)
      (if (empty? rest)
          total
          (word-length-iter (bf rest) (+ total 1))))
    (if (> (word-length-iter word 0) n)
        #f
        #t))
  
  (define (word-doc-mapper input-key-value-pair)
    (define (helper rest-of-line)
      (cond ((empty? rest-of-line)
             '())
            ((word-length-less-than-n? (car rest-of-line) n)
             (cons (cons (car rest-of-line)
                         (car input-key-value-pair))
                   (helper (cdr rest-of-line))))
            (else
             '())))
   (helper (cdr input-key-value-pair)))

  (define (word-docs-reduce sofar new)
     (cond ((empty? new)
            sofar)
           ((not (member? new sofar))
            (cons new sofar))
           (else
            sofar)))
  (mapreduce word-doc-mapper ; mapper
             word-docs-reduce ; reducer
             '() ; base case
             directory))

;2a (from-address to-address subject body)
 (mapreduce subject-count-mapper ; mapper
             subject-count-reducer ; reducer
             0 ; base case
             directory)
(define (subject-count-mapper input-key-value-pair)
  (cons (caddr input-key-value-pair) 1))
(define subject-count-reducer +)

;2b
 (mapreduce sort-mapper ; mapper
             sort-reducer ; reducer
             '() ; base case
             directory)
(define (subject-count-mapper input-key-value-pair)
  (cons (cdr input-key-value-pair) (car input-key-value-pair)))
(define (subject-count-reducer sofar new)
  (cons new sofar))

;2c
(mapreduce spam-email-mapper ; mapper
             + ; reducer
             0 ; base case
             directory)
(define (spam-email-mapper input-key-value-pair)
  (if (spam-subject? spam-list (caddr input-key-value-pair))
      (cons (car input-key-value-pair) 1)
      '()))
(define (spam-subject? spam-list subject)
  (if (member? subject spam-list)
      #t
      #f))
;3 map-reduce is an abstraction for a lot of parallel processing that we don't have to write
; so it is still good to use it. Unless map isn't parrallelizable in which case as seperate code
; simply optimized to run fast works

;4; i can imagine a way to parrallelize the sieve since if you have sieved up to p, the everything
;left to 2p must be prime, so each computer could strip the number and the agregate what is left, with
;one process for each prime (maybe with smaller primes getting help since they have to sieve more
; numbers)