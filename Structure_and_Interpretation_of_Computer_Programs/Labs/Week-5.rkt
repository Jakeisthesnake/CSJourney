#lang simply-scheme

;2.25
(car (cdr (car (cdr (cdr '(1 3 (5 7) 9))))))
(car (car '((7))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr '(1 (2 (3 (4 (5 (6 7))))))))))))))))))

;2.53
(newline)
(display "2.53")
(newline)
(define (memq item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(list 'a 'b 'c) ; '(a b c)
(list (list 'george)) ;'((george))
(cdr '((x1 x2) (y1 y2))) ; '(y1 y2) WRONG '((y1 y2)), because
;(cdr '((x1 . (x2 . ())) . ((y1 . (y2 . ())) . ()))) is ((y1 . (y2 . ())) . ()) which simplifies to '((y1 y2))
(cadr '((x1 x2) (y1 y2))) ; 'y1 WRONG '(y1 y2) becase car of the above is (y1 . (y2 . ())) which
; simplifies to '(y1 y2)
(pair? (car '(a short list))) ;#f
(memq 'red '((red shoes) (blue socks))) ;#f
(memq 'red '(red shoes blue socks)) ;#t Whoops, answer is the item which is '(red shoes blue socks)

;2.55
(car ''abracadabra) ;quote is the first element of the list 'abracadabra. But it would be ambiguous
;if it returened ', so it returns quote to indicate the word ' not the symbol '

;3 2.27
(newline)
(display "3 2.27")
(newline)
(define (deep-reverse lis)
  (define (deep-reverse-iter orig rev)
    (if (equal? (cdr orig) '())
        (if (word? (car orig))
            (cons (car orig) rev)
            (cons (deep-reverse (car orig)) rev))
        (if (word? (car orig))
            (deep-reverse-iter (cdr orig) (cons (car orig) rev))
            (deep-reverse-iter (cdr orig) (cons (deep-reverse (car orig)) rev)))))
  (deep-reverse-iter lis '()))
(deep-reverse '((1 () 3) (4 5 6 (7 8))) )
(deep-reverse '(1 2 3))