;2.74
;a (get-record) each employee's data is tagged with the devision name that then causes get-record
;to call a generic get-record function that pulls the appropriate get-record function specific to the
;file structure of the division where the emplyee works.

;b (get-salary) again pulls the tag from the employees record that says which division they work in.
;It then looks up the appropiate get-salary function per the devision structure and uses that function
;to navigate the employees record and get the right data

;c (find-employee-record) searches the companies various divisions data bases by recursively calling
;search on each devision, this search function looks up the approptiate function to search the
;specific division's database and return the record if found or passes on to search the next database
;after once again looking up the appropriate search function for the next database.

;d the lookup table will need get-record, get-salary, search for the new companies data base.


;2.75
(define (make-from-mag-angle mag angle)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* mag (cos angle)))
          ((eq? op 'imag-part) (* mag (sin angle)))
          ((eq? op 'magnitude) mag)
          ((eq? op 'angle) angle)
          (else (error "Unknown op: MAKE-FROM-MAG-ANGLE" op))))
  dispatch)


;2.76
; For explicit distpatching adding a new type every generic function needs to be updated to handle
; the new data type. Adding a new function only requires programing to handle all of the type tags.

; For data directed add a new type requires that the table needs to be extended to handle the new
; data i.e. that data specific flavors of each operation be added to the table. Adding a new
; operation requires adding dictinct operations to the table for each data type.

; For message passing, adding a new type requres that each operation must be updated to handle the
; new data type. Adding a new operation just requires that it is compatible with each existion data
; type.

; Data directed is best when there is a lot of adding new types since only that data type is touched
; (i.e. you don't have to worry about messing up the other data type functions). Message passing is
; best when you are adding new functions since each function is contained and you don't have to touch
; the other data type code.

;Corrections: Message passing is inappropriate when you need to add new operators as there the need
; to modify each type.

;2.77
; Without Allyssa's definitions, there isn't a way to interface with the complex tag for magnitude.
; Once it is added there will be a call to generic for magnitude will recursively call magnitude on the
; rectangular data, which will call generic to fetch the tag type rectangular and the magnitude
; defined in the rectangular package which gives the proper definiition. So apply is called twice.

;2.79
;First add the following to the generic package:
;(define (div x y) (apply-generic 'div x y))
;Then add the following to the complex package:
;(put 'eq? '(complex complex)
;   (lambda (z1 z2) (eq? z1 z2))) no tag since bool
;and
;(define (eq? z1 z2)
;  (and (= (real-part z1) (real-part z2))
;       (= (imag-part z1) (imag-part z2))))
;Then put the following into the rat package:
;(put 'eq? '(rational rational)
;  (lambda (x y) (eq? x y))) ;no tag since bool
;and
;(define (eq? x y)
;  (and (= (numer x) (numer y))
;       (= (denom x) (denom y))))
;Lastly put the following into the scheme-number package
;(put 'eq? '(scheme-number scheme-number)
;  (lambda (x y) (= x y)))

;2.80
;First add the following to the generic package:
;(define (=zero? x y) (apply-generic '=zero? x y))
;Then add the following to the complex package:
;(put '=zero? '(complex)
;   (lambda (z) (=zero? z))) no tag since bool
;and
;(define =zero? z)
;  (and (= (real-part z) 0)
;       (= (imag-part z1) 0)))
;Then put the following into the rat package:
;(put 'eq? '(rational)
;  (lambda (x) (=zero? x))) ;no tag since bool
;and
;(define (=zero? x)
;  (and (= (numer x) 0)
;       (= (denom x) 0))) ; WRONG should be blank since denominator doesn't matter
;Lastly put the following into the scheme-number package
;(put '=zero? '(scheme-number)
;  (lambda (x) (= x 0)))


;2.80
;a apply-generic will just loop as it keeps coercing the first argument to itself.
;b Louis is wrong because if an operation is defined for objects of the same type, then it won't
; try to coerce them. And if an operation isn't defined for the objects, coercing them to themselves
; won't help. However, it could be useful to check if the operation is defined for any of the types
; that the arguements could be coerced to.
;c 
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (cond (proc (apply proc (map contents args)))
          (if (= (length args) 2)
              (cond ((not (equal? (car type-tags) (cadr type-tags)))
                     (let ((type1 (car type-tags))
                           (type2 (cadr type-tags))
                           (a1 (car args))
                           (a2 (cadr args)))
                       (let ((t1->t2 (get-coercion type1 type2))
                             (t2->t1 (get-coercion type2 type1)))
                         (cond (t1->t2
                                (apply-generic op (t1->t2 a1) a2))
                               (t2->t1
                                (apply-generic op a1 (t2->t1 a2)))
                               (else (error "No method for these types"
                                            (list op type-tags)))))))
                    (else (error "No method for these types"
                                 (list op type-tags))))
              (error "No method for these types"
                     (list op type-tags)))))))

;2.83
;First add the following to the generic package:
(define (raise num) (apply-generic 'raise z))

;Then add
(put raise '(real)
  (lambda (x) (make-from-real-imag x 0))) ;assumes that make-from-real-imag
; adds a complex tag
;and
(put raise '(rational)
  (lambda (x) (make-real (/ (* 1.0 (numer x)) (denom x)) ))) ;assumes that make-real adds a
; read tag and that numer and denom are public
;and
(put raise '(scheme-number)
  (lambda (x) (make-rat x 1))) ;assumes that make-rat adds a rat tag