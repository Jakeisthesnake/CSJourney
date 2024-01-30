#lang simply-scheme

(define (get-method give-up-name message . objects)
  (if (null? objects)
      (no-method give-up-name)
      (let ((method ((car objects) message)))
	(if method
	    method
	    (apply get-method (cons give-up-name
				    (cons message (cdr objects)) ))))))


(define (ask object message . args)
  (let ((method (object message)))
    (if method
	(apply method args)
	(error "No method " message " in class " (cadr method)))))



;; ADV.SCM
;; This file contains the definitions for the objects in the adventure
;; game and some utility procedures.

(define place
  (let ((list-of-places '())) ;; class vars set up
    (lambda (class-message) ;; class dispatch proc
      (cond((eq? class-message 'list-of-places)
            (lambda () list-of-places))
           ((eq? class-message 'instantiate)
            (lambda (name) ;; Instantiation vars
              (let ((self '())
                    (directions-and-neighbors '())
                    (things '())
                    (people '())
                    (entry-procs '())
                    (exit-procs '())) ;; Instance vars
                (define (dispatch message) ;; Object dispatch proc
                  (cond ((eq? message 'type)
                         (lambda () 'place))
                        ((eq? message 'neighbors)
                         (lambda () (map cdr directions-and-neighbors)))
                        ((eq? message 'exits)
                         (lambda () (map car directions-and-neighbors)))
                        ((eq? message 'look-in)
                         (lambda (direction)
                           (let ((pair (assoc direction directions-and-neighbors)))
                             (if (not pair)
                                 '() ;; nothing in that direction
                                 (cdr pair)))))
                        ((eq? message 'appear)
                         (lambda (new-thing)
                           (if (memq new-thing things)
                               (error "Thing already in this place" (list name new-thing))
                               #f)
                           (set! things (cons new-thing things))
                           'appeared) )
                        ((eq? message 'enter)
                         (lambda (new-person)
                           (if (memq new-person people)
                               (error "Person already in this place" (list name new-person))
                               #f)
                           (set! people (cons new-person people))
                           (for-each (lambda (proc) (proc)) entry-procs)
                           'appeared))
                        ((eq? message 'gone)
                         (lambda (thing)
                            (if (not (memq thing things))
                                (error "Disappearing thing not here" (list name thing))
                                #f)
                           (set! things (delete thing things))
                           'disappeared))
                        ((eq? message 'exit)
                         (lambda (person)
                           (for-each (lambda (proc) (proc)) exit-procs)
                           (if (not (memq person people))
                               (error "Disappearing person not here" (list name person))
                               #f)
                           (set! people (delete person people))
                           'disappeared) )
                        ((eq? message 'new-neighbor)
                         (lambda (direction neighbor)
                           (if (assoc direction directions-and-neighbors)
                               (error "Direction already assigned a neighbor" (list name direction))
                               #f)
                           (set! directions-and-neighbors
                                 (cons (cons direction neighbor) directions-and-neighbors))
                           'connected))
                        ((eq? message 'add-entry-procedure)
                         (lambda (proc) (set! entry-procs (cons proc entry-procs)) ))
                        ((eq? message 'add-exit-procedure)
                         (lambda (proc) (set! exit-procs (cons proc exit-procs)) ))
                        ((eq? message 'remove-entry-procedure)
                         (lambda (proc) (set! entry-procs (delete proc entry-procs)) ))
                        ((eq? message 'remove-entry-procedure)
                         (lambda (proc) (set! entry-procs (delete proc entry-procs)) ))
                        ((eq? message 'clear-all-procs)
                         (lambda ()
                           (set! exit-procs '())
                           (set! entry-procs '())
                           'cleared))
                        (else ;; Else clause:
                         (let
                             ((method (get-method 'thief message)))
                           (if method ;; Try delegating...method
                               method
                               #f))))) ;; default-method
                (if (eq? self '()) ;; Initialize method:
                    (begin (set! self dispatch)
                           (set! list-of-places ;; user's init code
                                 (cons name list-of-places)))
                    #f)
                self))) ;; Class' instantiate;; proc returns object
           (else (error "Bad message to class" class-message))))))

;(define-class (place name)
;  (instance-vars
;   (directions-and-neighbors '())
;   (things '())
;   (people '())
;   (entry-procs '())
;   (exit-procs '()))
;  (method (type) 'place)
;  (method (neighbors) (map cdr directions-and-neighbors))
;  (method (exits) (map car directions-and-neighbors))
;  (method (look-in direction)
;    (let ((pair (assoc direction directions-and-neighbors)))
;      (if (not pair)
;	  '()                     ;; nothing in that direction
;	  (cdr pair))))           ;; return the place object
;  (method (appear new-thing)
;    (if (memq new-thing things)
;	(error "Thing already in this place" (list name new-thing))
;        #f)
;    (set! things (cons new-thing things))
;    'appeared)
;  (method (enter new-person)
;    (if (memq new-person people)
;	(error "Person already in this place" (list name new-person))
;        #f)
;    (set! people (cons new-person people))
;    (for-each (lambda (proc) (proc)) entry-procs)
;    'appeared)
;  (method (gone thing)
;    (if (not (memq thing things))
;	(error "Disappearing thing not here" (list name thing))
;        #f)
;    (set! things (delete thing things)) 
;    'disappeared)
;  (method (exit person)
;    (for-each (lambda (proc) (proc)) exit-procs)
;    (if (not (memq person people))
;	(error "Disappearing person not here" (list name person))
;        #f)
;    (set! people (delete person people)) 
;    'disappeared)

;  (method (new-neighbor direction neighbor)
;    (if (assoc direction directions-and-neighbors)
;	(error "Direction already assigned a neighbor" (list name direction))
;        #f)
;    (set! directions-and-neighbors
;	  (cons (cons direction neighbor) directions-and-neighbors))
;    'connected)
;
;  (method (add-entry-procedure proc)
;    (set! entry-procs (cons proc entry-procs)))
;  (method (add-exit-procedure proc)
;    (set! exit-procs (cons proc exit-procs)))
;  (method (remove-entry-procedure proc)
;    (set! entry-procs (delete proc entry-procs)))
;  (method (remove-exit-procedure proc)
;    (set! exit-procs (delete proc exit-procs)))
;  (method (clear-all-procs)
;    (set! exit-procs '())
;    (set! entry-procs '())
;    'cleared) )

(define person
  (let ((list-of-people '())) ;; class vars set up
    (lambda (class-message) ;; class dispatch proc
      (cond((eq? class-message 'instantiate)
            (lambda (name place) ;; Instantiation vars
              (let ((self '()) ;; Instance vars
                    (possessions '())
                    (saying ""))
                (define (dispatch message) ;; Object dispatch proc
                  (cond ((eq? message 'type)
                         (lambda () 'person))
                        ((eq? message 'look-around)
                         (lambda () (map (lambda (obj) (ask obj 'name))
                                         (filter (lambda (thing) (not (eq? thing self)))
                                                 (append (ask place 'things) (ask place 'people))))))
                        ((eq? message 'take)
                         (lambda (thing)
                           (cond ((not (thing? thing)) (error "Not a thing" thing))
                                 ((not (memq thing (ask place 'things)))
                                  (error "Thing taken not at this place"
                                         (list (ask place 'name) thing)))
                                 ((memq thing possessions) (error "You already have it!"))
                                 (else
                                  (announce-take name thing)
                                  (set! possessions (cons thing possessions));; If somebody already has this object...
                                  (for-each
                                   (lambda (pers)
                                     (if (and (not (eq? pers self)) ; ignore myself
                                              (memq thing (pers 'possessions)))
                                         (begin
                                           ((pers 'lose) thing)
                                           (have-fit pers))
                                         #f))
                                   ((place 'people)))
                                  ((thing 'change-possessor) self)
                                  'taken))))
                        ((eq? message 'lose)
                         (lambda (thing)
                           (set! possessions (delete thing possessions))
                           ((thing 'change-possessor) 'no-one)
                           'lost) )
                        ((eq? message 'talk)
                         (lambda () (print saying) ))
                        ((eq? message 'set-talk)
                         (lambda (string) (set! saying string)))
                        ((eq? message 'exits)
                         (lambda () (ask place 'exits)))
                        ((eq? message 'notice)
                         (lambda (person) (ask self 'talk)))
                        ((eq? message 'go)
                         (lambda (direction)
                                  (let ((new-place (ask place 'look-in direction)))
                                    (cond ((null? new-place)
                                           (error "Can't go" direction))
                                          (else
                                           (ask place 'exit self)
                                           (announce-move name place new-place)
                                           (for-each
                                            (lambda (p)
                                              (ask place 'gone p)
                                              (ask new-place 'appear p))
                                            possessions)
                                           (set! place new-place)
                                           (ask new-place 'enter self))))))
                         (else ;; Else clause:
                           (let ((method (get-method message)))
                             (if method ;; Try delegating...method
                                 method
                                 #f))))) ;; default-method
                (if (eq? self '()) ;; Initialize method:
                    (begin (set! self dispatch)
                           (ask place 'enter self))
                    #f)
                self))) ;; Class' instantiate;; proc returns object
            (else (error "Bad message to class" class-message))))))
;
;(define-class (person name place)
;  (instance-vars
;   (possessions '())
;   (saying ""))
;  (initialize
;   (ask place 'enter self))
;  (method (type) 'person)
;  (method (look-around)
;    (map (lambda (obj) (ask obj 'name))
;	 (filter (lambda (thing) (not (eq? thing self)))
;		 (append (ask place 'things) (ask place 'people)))))
;  (method (take thing)
;    (cond ((not (thing? thing)) (error "Not a thing" thing))
;	  ((not (memq thing (ask place 'things)))
;	   (error "Thing taken not at this place"
;		  (list (ask place 'name) thing)))
;	  ((memq thing possessions) (error "You already have it!"))
;	  (else
;	   (announce-take name thing)
;	   (set! possessions (cons thing possessions))
;	       
;	   ;; If somebody already has this object...
;	   (for-each
;	    (lambda (pers)
;	      (if (and (not (eq? pers self)) ; ignore myself
;		       (memq thing (ask pers 'possessions)))
;		  (begin
;		   (ask pers 'lose thing)
;		   (have-fit pers))
;                  #f))
;	    (ask place 'people))
;	       
;	   (ask thing 'change-possessor self)
;	   'taken)))

;  (method (lose thing)
;    (set! possessions (delete thing possessions))
;    (ask thing 'change-possessor 'no-one)
;    'lost)
;  (method (talk) (print saying))
;  (method (set-talk string) (set! saying string))
;  (method (exits) (ask place 'exits))
;  (method (notice person) (ask self 'talk))
;  (method (go direction)
;    (let ((new-place (ask place 'look-in direction)))
;      (cond ((null? new-place)
;	     (error "Can't go" direction))
;	    (else
;	     (ask place 'exit self)
;	     (announce-move name place new-place)
;	     (for-each
;	      (lambda (p)
;		(ask place 'gone p)
;		(ask new-place 'appear p))
;	      possessions)
;	     (set! place new-place)
;	     (ask new-place 'enter self))))) )


(define (no-method name)
  (list 'no-method name))

(define thing
  (let ()
    (lambda (class-message)
      (cond
       ((eq? class-message 'instantiate)
	(lambda (name)
	  (let ((self '())
                (possessor 'no-one))
	    (define (dispatch message)
	      (cond
	       ((eq? message 'usual)
		(error "Can't use USUAL without a parent." 'thing))
	       ((eq? message 'name) (lambda () name))
	       ((eq? message 'possessor) (lambda () possessor))
	       ((eq? message 'type) (lambda () 'thing))
	       ((eq? message 'change-possessor)
		(lambda (new-possessor)
		  (set! possessor new-possessor)))
	       (else #f)))
	    (if (equal? self '())
		(set! self dispatch)
                #f)
            self)))
        (else (error "Bad message to class" class-message))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implementation of thieves for part two
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define *foods* '(pizza potstickers coffee))

(define (edible? thing)
  (member (ask thing 'name) *foods*))

(define theif
  (let ((list-of-thieves '())) ;; class vars set up
    (lambda (class-message) ;; class dispatch proc
      (cond ((eq? class-message 'instantiate)
             (lambda (name initial-place) ;; Instantiation vars
               (let ((self '()) ;; Instance vars
                     (behavior 'steal)
                     (my-person (ask person 'instantiate name initial-place)))
                 (define (dispatch message) ;; Object dispatch proc
                   (cond ((eq? message 'usual) ;; How USUAL works
                          (lambda (message . args)
                            (let ((method (get-method 'theif message my-person)))
                              (if method
                                  (apply method args)
                                  (error "No USUAL method" message 'banana-holder)))))
                         ((eq? message 'type)
                          (lambda () 'thief))
                         ((eq? message 'notice)
                          (lambda (person)
                            (if (eq? behavior 'run)
                                (ask self 'go (pick-random (ask self 'place 'exits)))
                                (let ((food-things
                                       (filter (lambda (thing)
                                                 (and (edible? thing)
                                                      (not (eq? (ask thing 'possessor) self))))
                                               (ask 'place 'things))))
                                  (if (not (null? food-things))
                                      (begin
                                        (ask self 'take (car food-things))
                                        (set! behavior 'run)
                                        (ask self 'notice person)) )))))
                         (else ;; Else clause:
                          (let
                              ((method (get-method 'theif message my-person)))
                            (if method ;; Try delegating...method
                                method
                                #f))))) ;; default-method 
                 (if (eq? self '()) ;; Initialize method:
                     (begin (set! self dispatch)
                            (set! list-of-banana-holders ;; user's init code
                                  (cons name list-of-banana-holders)))
                     #f)
                 self))) ;; Class' instantiate;; proc returns object
           (else (error "Bad message to class" class-message))))))

;(define-class (thief name initial-place)
;  (parent (person name initial-place))
;  (instance-vars
;   (behavior 'steal))
;  (method (type) 'thief)
;
;  (method (notice person)
;    (if (eq? behavior 'run)
;	(ask self 'go (pick-random (ask (usual 'place) 'exits)))
;	(let ((food-things
;	       (filter (lambda (thing)
;			 (and (edible? thing)
;			      (not (eq? (ask thing 'possessor) self))))
;		       (ask (usual 'place) 'things))))
;	  (if (not (null? food-things))
;	      (begin
;	       (ask self 'take (car food-things))
;	       (set! behavior 'run)
;	       (ask self 'notice person)) )))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; this next procedure is useful for moving around

(define (move-loop who)
  (newline)
  (print (ask who 'exits))
  (display "?  > ")
  (let ((dir (read)))
    (if (equal? dir 'stop)
	(newline)
	(begin (print (ask who 'go dir))
	       (move-loop who)))))


;; One-way paths connect individual places.

(define (can-go from direction to)
  (ask from 'new-neighbor direction to))


(define (announce-take name thing)
  (newline)
  (display name)
  (display " took ")
  (display (ask thing 'name))
  (newline))

(define (announce-move name old-place new-place)
  (newline)
  (newline)
  (display name)
  (display " moved from ")
  (display (ask old-place 'name))
  (display " to ")
  (display (ask new-place 'name))
  (newline))

(define (have-fit p)
  (newline)
  (display "Yaaah! ")
  (display (ask p 'name))
  (display " is upset!")
  (newline))


(define (pick-random set)
  (nth (random (length set)) set))

(define (delete thing stuff)
  (cond ((null? stuff) '())
	((eq? thing (car stuff)) (cdr stuff))
	(else (cons (car stuff) (delete thing (cdr stuff)))) ))

(define (person? obj)
  (and (procedure? obj)
       (member? (ask obj 'type) '(person police thief))))

(define (thing? obj)
  (and (procedure? obj)
       (eq? (ask obj 'type) 'thing)))