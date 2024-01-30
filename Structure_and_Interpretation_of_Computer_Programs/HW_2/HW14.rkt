#lang sicp 

;4.25
;Because of applicative order, it won't stop at the base case and will evauluate fact indefinitely
;(although it will always be 0 mathmatically)

;With lazy evaluation, 


;4.26

(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value)) 


Ok, I might ahve this wrong since I think Ben is just saying use the if part of the def if you need
an unless. Yep, got it wrong, Ben was saying you could add this to the evaluator using
(define (unless->if exp)
  (make-if (unless-predicate exp)
	   (unless-consequent exp)
	   (unless-alternative exp)))

(define unless-predicate cadr)
(define unless-alternative caddr)
(define unless-consequent cadddr)


But Alyssa is saying if you, for example, make all of your error handling with that and than your
want to change how errors are handled, you have to change each one instead of the the dedicated error
unless... Ohhh what Alyssa was actually saying is that you can't pass special forms in functions.

So a case where this could be nice... hmmm maybe a scenario where there is a process that runs on a
data set but a subset is much mort likely to have an anomoly. It is ideal to catch all of the anomolies
but it is alright to let some pass through undetected. Function A could identify if a datum was in the
subset and pass Function B if it was not, and the unless if it was in the subset to apply a more
comprehensive anaylis of the datam and handle it differently if it was indeed an anomoly. For speed
sake, the anaylis would only be done on items in the subset.


;4.28
;it is needed when a thunk is passed as the procedure to an application
;a thunk is make when there are arguments to a non-primative procedure
;specifically the thunks are made of the arguments to the non-primative procedure
;so when argument of a non primative procedure needs to be evaluated
(square (+ 1 2))





;4.42
(define (correct-rank)
  (let ((Betty (amb 1 2 3 4 5))
        (Ethel (amb 1 2 3 4 5))
        (Joan (amb 1 2 3 4 5))
        (Mary (amb 1 2 3 4 5))
        (Kitty (amb 1 2 3 4 5)))
    (require
      (distinct? (list Betty Ethel Joan Mary Kitty)))
    (require (or (= Kitty 2) (= Betty 3)))
    (require (or (= Ethel 1) (= Joan 2)))
    (require (or (= Joan 3) (= Ethel 5)))
    (require (or (= Kitty 2) (= Mary 4)))
    (require (or (= Mary 4) (= Betty 1)))
    (list (list 'Kitty Kitty)
          (list 'Ethel Ethel)
          (list 'Joan Joan)
          (list 'Betty Betty)
          (list 'Mary Mary))))

;4.45
parse '(the professor lectures to the student in the class with the cat))
(sentence
 (simple-noun-phrase (article the) (noun professor))
 (verb-phrase
  (verb-phrase
   (verb-phrase (verb lectures) (prep-phrase (prep to) (simple-noun-phrase (article the) (noun student))))
   (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class))))
  (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))

(sentence
 (simple-noun-phrase (article the) (noun professor))
 (verb-phrase
  (verb-phrase (verb lectures) (prep-phrase (prep to) (simple-noun-phrase (article the) (noun student))))
   (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class))))
  (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))

;4.49, 4.50, 4.52