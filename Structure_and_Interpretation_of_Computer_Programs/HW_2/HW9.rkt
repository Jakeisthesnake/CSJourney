#lang sicp


;3.16


(define (last-pair x)
(if (null? (cdr x))
    x
    (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
;3 a list of 3 pairs cdr x -> x1, cdr x1 -> x2, cdr x2 null
;4 car x -> x1, cdr x -> x2, cdr x1 -> x2, cdr x2 null (x2 counted 2)
;7 car x -> x1, cdr x -> x1, car x1 -> x2, cdr x1 -> x2, cdr x2 null (x1 counted 2, x2 counted 4 times)
;inf cycle where cdr x2 -> x
(define a '(1 2 3))
;(count-pairs a) --> 3

(define b (list 1 2 3))
(set-car! (cdr b) (cddr b))
;(count-pairs b); --> 4

(define x (list 1))
(define y (cons x x))
(define c (cons y y))
;(count-pairs c); --> 7

(define d (make-cycle (list 1 2 3)))
;(count-pairs d); --> infinite loop

;3.17
;not sure how to identify the name of a pair, so I am just going to break the structure as I go while
;copying it into a new structure

;not perfect, I think there is some funny business with how things are set inside functions as I
;can't get x to reset at the end...

;ok, remembering that eq? and memq look for exact equality would have made this a lot easier

(define (count-pairs-correctly x)
  (define (count-pairs-rec strct-parent-copy strct car-or-cdr) ; data = '(strct strct-copy num-pairs)
    ;(display strct-parent-copy)
    ;(newline)
    ;(display strct)
    ;(newline)
    ;(newline)
    (cond ((not (pair? strct)) 0)
          ((equal? (car strct) 'tagged)
           (if (equal? car car-or-cdr)
               (begin
                 (set-car! strct-parent-copy (cdr strct))
                 0)
               (begin
                 (set-cdr! strct-parent-copy (cdr strct))
                 0)))
          (else
            (let ((strct-copy (cons (car strct) (cdr strct))))
              (set-car! strct 'tagged)
              (set-cdr! strct strct-copy)
              (if (equal? car car-or-cdr)
                  (begin
                    (set-car! strct-parent-copy strct-copy)
                    0)
                  (begin
                    (set-cdr! strct-parent-copy strct-copy)
                    0))
              (+ 1 (count-pairs-rec strct-copy (car strct-copy) car) (count-pairs-rec strct-copy (cdr strct-copy) cdr))))))
  (let ((x-copy-parent (cons '() x)))
    (display x)
    (let ((count (count-pairs-rec x-copy-parent x car)))
      (display x)
      (newline)
      (display (car x-copy-parent))
      (newline)
      (set! x 1)
      (display x) ; shows the right thing, but a isn't asseigned properly... I guess x is bound
      (newline)
      count)))
a
(count-pairs-correctly a)
a
b
(count-pairs-correctly b)
b
d
(count-pairs-correctly d)
d
 


;3.21
;The way scheme processes list structures causes the end pointer to b to show up as a second b.


(define (print-queue que)
  (cond ((and (not (pair? que)) (pair? (car que)))
         (error "Not a queue"))
        (else
          (car que))))

(define (make-queue) (cons '() '()))
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))
(define (empty-queue? queue)
  (null? (front-ptr queue)))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
       (cond ((empty-queue? queue)
              (set-front-ptr! queue new-pair)
              (set-rear-ptr! queue new-pair)
              queue)
             (else
              (set-cdr! (rear-ptr queue) new-pair)
              (set-rear-ptr! queue new-pair)
              queue))))
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue)))
(define q1 (make-queue))
(insert-queue! q1 'a)
(insert-queue! q1 'b)
(delete-queue! q1)
(delete-queue! q1)

(print-queue q1)


;3.25
(define (lookup table keylist)
  (let ((subtable
         (assoc (car keylist) (cdr table))))
    (if subtable
        (if (null? (cdr keylist))
            (cdr subtable)
            (lookup subtable (cdr keylist)))
        #f)))


(define (insert! table value keylist)
  (define (add-table-keys value keylist)
    (if (null? (cdr keylist))
        (cons (car keylist) value)
        (cons (car keylist)
              (cons (add-table-keys value (cdr keylist))
                    '()))))

  (let ((subtable (assoc (car keylist) (cdr table))))
    (if subtable
        (if (null? (cdr keylist))
            (set-cdr! subtable value)
            (insert! subtable value (cdr keylist)))
        (set-cdr! table (cons (add-table-keys value keylist) (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))


;3.27
;Wow that diagram was a pain. the most steps a particular n takes is lookup n-1 and n-2 in a table
;except for 0 and 1 which return defined answers from the lambda function that computes each successive
;fib num. Thus it is theta n. Memoize needed to have a seperate function to allow each memo-fib to 
;reference the same table, so no it wouldn't have worked.

;vector-append
(define (vector-append v1 v2)
  (let ((a (vector-length v1))
        (b (vector-length v2))
        (c (+ (vector-length v1) (vector-length v2))))
    (define vec (make-vector c))
    (define (vector-append-iter i)
      (cond ((< i a)
             (vector-set! vec  i (vector-ref v1 i))
             (vector-append-iter (+ i 1)))
            ((< i (- c 1))
             (vector-set! vec  i (vector-ref v2 (- i a)))
             (vector-append-iter (+ i 1)))
            ((= i (- c 1))
             (vector-set! vec  i (vector-ref v2 (- i a)))
             vec)))
    (vector-append-iter 0)))

(define va (vector 1 2 3))
(define vb (vector 1 2 3 4 2 33))
(vector-append va vb)

;vector-filter
;#lang simply-scheme
#| (define (vector-filter pred vec)
  (define c (vector-length vec))
  (define count 0)
  (define newvec-temp (make-vector c))
  (define (vector-filter-iter i)
    (cond ((= i c) 'done)
          ((pred (vector-ref vec i))
           (vector-set! newvec-temp count (vector-ref vec i))
           (set! count (+ count 1))
           (vector-filter-iter (+ i 1)))
          (else (vector-filter-iter (+ i 1)))))
  (vector-filter-iter 0)
  (define newvec (make-vector count))
  (define (vector-swap-iter j)
    (if (= j count)
        newvec
        (begin
          (vector-set! newvec j (vector-ref newvec-temp j))
          (vector-swap-iter (+ j 1)))))
  (vector-swap-iter 0))
      
(define (test x)
  (< x 4))

(vector-filter test (vector 1 2 3 4 5 4 3 2 2 4 2)) |#

;bubble-sort
(define (bubble-sort! vec)
  (define c (vector-length vec))
  (define temp 0)
  (define (bubble-swap vec1 i n)
    (cond ((= n i)
           vec1)
          ((> (vector-ref vec1 i) (vector-ref vec1 (+ i 1)))
           (set! temp (vector-ref vec1 i))
           (vector-set! vec1 i (vector-ref vec1 ( + i 1)))
           (vector-set! vec1 (+ i 1) temp)
           (bubble-swap vec1 (+ i 1) n))
          (else
           (bubble-swap vec1 (+ i 1) n))))


  (define (bubble-sort-rows j)
    (if (= j 0)
        'done
        (begin
          (bubble-swap vec 0 j)
          (bubble-sort-rows (- j 1)))))
  (bubble-sort-rows (- c 1)))

(define testvec (vector 1 2 3 5 4 3 2 5 4 3 2 1))
(bubble-sort! testvec)
testvec

;Lemma: swaping each successive pair leaves the largest at the end.
; assume false: the largest isn't at the end and that there is a smaller element to the right.
; This implies that there was step where a) the largest
; and the next in line were compared and not swapped, or b) the largest and the one before were
; compared and swapped. However by definition of the algorithm, elements are swapped if and only iff
; the larger is to the left, so neighter a nor be could have taken place. Thus the largest must be at
; the end, proving the lemma.
; By induction: the base case 0 elements left out (i.e. the entire vector) by the lemma, the largest
; (or equally largest) element is at the end and is thus greater than everything to the right.
; The n+1 element left out. Given the largest n elements are to the right, thus, but applying the
; the vector with n+1 elements left out will have the largest of that batch to the rightmost spot,
; thus all n+1 largest elements will be to the right and be larger than all elements to the left. This
; can be repeated until n = length of the vector.

;theta of n^2