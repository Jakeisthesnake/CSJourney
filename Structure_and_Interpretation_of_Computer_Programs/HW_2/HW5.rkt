#lang simply-scheme

;2.24
(list 1 (list 2 (list 3 4))) ; '(1 (2 (3 4)))
;(1|->A) A(->B|()) B(2|->C) C(->D|()) D(3|->E) E(4|())
;(1 (2 (3 4)))
;1    (2 (3 4))
;      2   (3 4)
;          3   4

;2.26
(define x (list 1 2 3))
(define y (list 4 5 6))
(append x y) ; '(1 2 3 4 5 6)
(cons x y) ; '((1 2 3) . (4 5 6)) WRONG '((1 2 3) 4 5 6)
(list x y) ;  '((1 2 3) (4 5 6))

;2.29
(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))

;(define (make-mobile left right)
;  (list left right))

;(define (make-branch length structure)
;  (list length structure))

;2.29a
(define (right-branch mobile)
  (cdr mobile))

(define (left-branch mobile)
  (car mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

;2.29b
(define (total-weight mobile)
  (if (number? mobile)
      mobile
      (+ (total-weight (branch-structure (right-branch mobile)))
         (total-weight (branch-structure (left-branch mobile))))))

;2.29c
(define (balanced? mobile)
  (if (number? mobile)
      #t
      (and (= (* (total-weight (branch-structure (right-branch mobile)))
                 (branch-length (right-branch mobile)))
              (* (total-weight (branch-structure (left-branch mobile)))
                 (branch-length (left-branch mobile))))
           (balanced? (branch-structure (right-branch mobile)))
           (balanced? (branch-structure (left-branch mobile))))))

(define l2a (make-branch 2 1))
(define r2a (make-branch 1 2))
(define l2b (make-branch 4 3))
(define r2b (make-branch 3 4))
(define m2a (make-mobile l2a r2a))
(define m2b (make-mobile l2b r2b))
(define l1a (make-branch 7 m2a))
(define r1a (make-branch 3 m2b))
(define m1a (make-mobile l1a r1a))
(total-weight m1a)
(balanced? m1a)

;2.29d
;Just need to change the cadr to cdr in the right side operations

;2.30
(define (square x)
  (if (number? x)
      (* x x)
      '()))

(define (square-tree tree)
  (cond ((equal? tree '())
         '())
        ((number? tree)
         (square tree))
        (else
         (cons (square-tree (car tree)) (square-tree (cdr tree))))))

(define (square-tree-map tree)
  (map square-tree tree))


(square-tree '((1 () 3) (4 5 6 (7 8))) )
(square-tree '(1 2 3))
(square-tree-map '((1 () 3) (4 5 6 (7 8))) )

;2.31
(define (tree-map fn tree)
  (define (tree-map-fn tree1)
    (if (pair? tree1)
        (cons (tree-map-fn (car tree1)) (tree-map-fn (cdr tree1)))
        (fn tree1)))
  (tree-map-fn tree))
(define (sq-tr tree) (tree-map square tree))
(sq-tr '((1 () 3) (4 5 6 (7 8))) )

;2.32
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (lis) (append (list (car s)) lis)) rest)))))
(subsets '(1 2 3))
; this works because is recurs down to the empty list and the builds up the possible sublists
; by adding one element from s at a time to a copy of each of the sublist and then appending those
; to the growin list of sub lists.
; In other words a list of sublists of s can be broken down into the list of elements with s1 and the
; list of elements without s1. This can be called recusively down to the empty list whereupon each
; sublist without s_i is built up.

;2.36
(define (accumulate op init lis)
  (if (null? lis)
      init
      (op (car lis)
          (accumulate op init (cdr lis)))))

(define (list-of-first-eles seqs)
  (if (empty? seqs)
      '()
      (cons (caar seqs) (list-of-first-eles (cdr seqs)))))
(list-of-first-eles '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

(define (list-of-butfirst-eles seqs)
  (if (empty? seqs)
      '()
      (cons (cdar seqs) (list-of-butfirst-eles (cdr seqs)))))
(list-of-butfirst-eles '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (list-of-first-eles seqs))
            (accumulate-n op init (list-of-butfirst-eles seqs)))))
(accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

;answer
(define (accumulate-n-ans op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n-ans op init (map cdr seqs)))))
(accumulate-n-ans + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

;2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (vec) (dot-product vec v)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (vec) (matrix-*-vector cols vec)) m)))

(define mat1 '((1 2 3) (4 5 6)))
(define mat2 '((1 2) (3 4)))
(define v1 '(1 2 3))
(define v2 '(1 2 3))
(dot-product v1 v2)
(matrix-*-vector mat1 v1)
(transpose mat1)
(matrix-*-matrix '((1 2)) '((1 2) (3 4)))
(matrix-*-matrix mat1 (transpose mat1)) 


; 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(accumulate / 1 (list 1 2 3)) ; 1/6 WRONG oh, (1/(2/(3/1))) = (1/(2/(3))) = (1/(2/3)) = (3/2) 
(fold-left / 1 (list 1 2 3)) ; 1/6
(accumulate list '() (list 1 2 3)) ;'(1 2 3 ()) WRONG '(1 (2 (3 ())))
(fold-left list '() (list 1 2 3)) ;'(((() 1 ) 2) 3)
; f(a, f(b, i)) = f(f(i, a), b); Partial credit: I was missing the obsevation that is the associative
; property (although in my opinion there is some funny business with the identity application)

;2.54
(define (equal1? a b)
  (cond ((and (symbol? a) (symbol? b))
         (eq? a b))
        ((and (number? a) (number? b))
         (= a b))
        ((and (empty? a) (empty? b))
         (eq? a b))
        ((and (list? a) (list? b))
         (and (equal1? (car a) (car b))
               (equal1? (cdr a) (cdr b))))
        (else #f)))
(equal1? 2 2)
(equal1? '2 '2)
(equal1? '(1 2) '(1 2))

#| Hmmmmm the answer has a lot more extra cases to check not sure why. I'll sleep on it.
(define (equal? a b)
  (cond ((and (symbol? a) (symbol? b)) (eq? a b))
	((or (symbol? a) (symbol? b)) #f)
	((and (number? a) (number? b)) (= a b))       ;; not required but
	((or (number? a) (number? b)) #f)             ;; suggested in footnote
	((and (null? a) (null? b)) #t)
	((or (null? a) (null? b)) #f)
	((equal? (car a) (car b)) (equal? (cdr a) (cdr b)))
	(else #f))) |#

;; Scheme calculator -- evaluate simple expressions

; The read-eval-print loop:
(define (calc)
  (display "calc: ")
  (flush-output)
  (print (calc-eval (read)))
  (newline)
  (calc))

; Evaluate an expression:

(define (calc-eval exp)
  (cond ((number? exp) exp)
        ((word? exp) exp)
    	((list? exp) (calc-apply (car exp) (map calc-eval (cdr exp))))
	(else (error "Calc: bad expression:" exp))))

; Apply a function to arguments:

(define (calc-apply fn args)
  ;(display args)
  ;(newline)
  (cond ((eq? fn '+) (accumulate + 0 args))
	((eq? fn '-) (cond ((null? args) (error "Calc: no args to -"))
			           ((= (length args) 1) (- (car args)))
			           (else (- (car args) (accumulate + 0 (cdr args))))))
	((eq? fn '*) (accumulate * 1 args))
	((eq? fn '/) (cond ((null? args) (error "Calc: no args to /"))
			           ((= (length args) 1) (/ (car args)))
			           (else (/ (car args) (accumulate * 1 (cdr args))))))
        ((eq? fn 'first) (cond ((null? args) (error "Calc: no args to first"))
                               ((empty? (cdr args)) (first (car args)))
                               (else (first args))))
        ((eq? fn 'butfirst) (cond ((null? args) (error "Calc: no args to butfirst"))
                               ((empty? (cdr args)) (butfirst (car args)))
                               (else (butfirst args))))
        ((eq? fn 'last) (cond ((null? args) (error "Calc: no args to last"))
                               ((empty? (cdr args)) (last (car args)))
                               (else (last args))))
        ((eq? fn 'butlast) (cond ((null? args) (error "Calc: no args to butlast"))
                               ((empty? (cdr args)) (butlast (car args)))
                               (else (butlast args))))
        ((eq? fn 'word) (cond ((null? args) (error "Calc: no args to word"))
                               ((empty? (cdr args)) (word (car args)))
                               (else (define (accumulate-word lis)
                                       (if (null? (cdr lis))
                                           (car lis)
                                           (word (car lis)
                                                 (accumulate-word (cdr lis)))))
                                     (accumulate-word args))))
	(else (error "Calc: bad operator:" fn))))
(calc)

