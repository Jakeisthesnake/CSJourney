#lang sicp


(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (analyze exp)
  (display "analyze ")(display exp)(newline)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((display "quoted ")(display exp)(newline)
         (quoted? exp) (analyze-quoted exp))
        ((variable? exp)(analyze-variable exp))
        ((assignment? exp)(analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp)(analyze-if exp))
        ((display "if-fail ")(display exp)(newline)
         (if-fail? exp)(analyze-if-fail exp))
        ((display "amb ")(display exp)(newline)
         (amb? exp) (analyze-amb exp))
        ((ramb? exp) (analyze-ramb exp))
        ((lambda? exp) (analyze-lambda exp))
        ((let? exp) (analyze-let exp))
        ((begin? exp)(analyze-sequence (begin-actions exp)))
        ((cond? exp)(analyze (cond->if exp)))
        ((display "app ")(display exp)(newline)
         (application? exp) (analyze-application exp))
        (else (error "Unknown expression type: ANALYZE" exp))))

;  (cond ((self-evaluating? exp) (display "self-eval ")(display exp)(newline) (analyze-self-evaluating exp))
;        ((quoted? exp) (display "quoted ")(display exp)(newline)(analyze-quoted exp))
;        ((variable? exp) (display "var ")(display exp)(newline)(analyze-variable exp))
;        ((assignment? exp) (display "assign ")(display exp)(newline)(analyze-assignment exp))
;        ((definition? exp) (display "def ")(display exp)(newline)(analyze-definition exp))
;        ((if? exp) (display "if ")(display exp)(newline)(analyze-if exp))
;        ((lambda? exp) (display "lambda ")(display exp)(newline)(analyze-lambda exp))
;        ((let? exp) (display "let ")(display exp)(newline)(analyze-let exp))
;        ((begin? exp) (display "begin ")(display exp)(newline)(analyze-sequence (begin-actions exp)))
;        ((cond? exp) (display "cond ")(display exp)(newline)(analyze (cond->if exp)))
;        ((application? exp) (display "app ")(display exp)(newline)(analyze-application exp))
;        (else (error "Unknown expression type: ANALYZE" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env) fail)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2) ; *1*
               (let ((old-value
                      (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda () ; *2*
                            (set-variable-value!
                             var old-value env)
                            (fail2)))))
             fail))))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             ;; success continuation for evaluating the predicate
             ;; to obtain pred-value
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             ;; failure continuation for evaluating the predicate
             fail))))


(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env) fail))))

(define (analyze-sequence exps)(define (sequentially a b)
                                 (lambda (env succeed fail)
                                   (a env
                                      ;; success continuation for calling a
                                      (lambda (a-value fail2)
                                        (b env succeed fail2))
                                      ;; failure continuation for calling a
                                      fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc
                            (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs)
       env
       ;; success continuation for this aproc
       (lambda (arg fail2)
         (get-args
          (cdr aprocs)
          env
          ;; success continuation for
          ;; recursive call to get-args
          (lambda (args fail3)
            (succeed (cons arg args) fail3))
          fail2))
       fail)))

(define (execute-application proc args succeed fail)
  (display proc)
  (newline)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment
           (procedure-parameters proc)
           args
           (procedure-environment proc))
          succeed
          fail))
        (else
         (error "Unknown procedure type: EXECUTE-APPLICATION"
                proc))))

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices)
             env
             succeed
             (lambda () (try-next (cdr choices))))))
      (try-next cprocs))))

(define (analyze-if-fail exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      ((car cprocs) env succeed (car cprocs)))))

(define (analyze-ramb exp)
  (let ((cprocs (map analyze (ramb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices)
             env
             succeed
             (lambda () (try-next (cdr choices))))))
      (try-next cprocs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;METACIRCULAR EVALUATOR FROM CHAPTER 4 (SECTIONS 4.1.1-4.1.4) of
;;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;;;Matches code in ch4.scm

;;;;This file can be loaded into Scheme as a whole.
;;;;Then you can initialize and start the evaluator by evaluating
;;;; the expression (mce).

;;;from section 4.1.4 -- must precede def of metacircular apply
(define apply-in-underlying-scheme apply)

;;;SECTION 4.1.1

(define (mc-eval exp env)
  ;(display exp)
  (cond ((self-evaluating? exp) exp)
	((variable? exp)  (lookup-variable-value exp env))
	((quoted? exp)   (text-of-quotation exp))
	((definition? exp)  (eval-definition exp env))
	((undef? exp)   (eval-undef exp env))
	((if? exp) (eval-if exp env))
	((lambda? exp)
         (make-procedure (lambda-parameters exp)
			 (lambda-body exp)
			 env))
        ((let? exp)
         ;(display "let")(newline)
         (mc-eval (let->lambda exp) env))
	((begin? exp) 
	 (eval-sequence (begin-actions exp) env))
	((and? exp)   (mc-eval (and->if exp) env))
	((or? exp) (mc-eval (or->if exp) env))
	((cond? exp)  (mc-eval (cond->if exp) env))
	((assignment? exp)  (eval-assignment exp env))
	((application? exp)  
	 (mc-apply (mc-eval (operator exp) env)
		   (list-of-values (operands exp) env)))
        (else
	 (error "Unknown expression type -- EVAL" exp))))

(define (mc-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))


(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (mc-eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (mc-eval (if-predicate exp) env))
      (mc-eval (if-consequent exp) env)
      (mc-eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (mc-eval (first-exp exps) env))
        (else (mc-eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (mc-eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (mc-eval (definition-value exp) env)
                    env)
  'ok)

(define (eval-undef exp env)
  (undef! (undef-variable exp) env)
  'ok)

;;;SECTION 4.1.2

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        ((boolean? exp) true)
	(else false)))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (variable? exp) (symbol? exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))


(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))


(define (undef? exp)
  (tagged-list? exp 'undef))

(define (undef-variable exp)
  (cadr exp))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (let? exp) (tagged-list? exp 'let))

(define (let-params exp) (map car (cadr exp)))
(define (let-args exp) (map cadr (cadr exp)))
(define (let-body exp) (caddr exp))
(define (let->lambda exp)
  (append (list (list 'lambda (let-params exp) (let-body exp))) (let-args exp))) 

(define (analyze-let exp)
  ;(display "analyze-let")
  ;(display (let-params exp))(newline)
  ;(display (let-args exp))(newline)
  ;(display (let-body exp))(newline)
  (let ((vars (let-params exp))
        (aproc (let-args exp))
        (bproc (let-body exp)))
    ;(display (append (list (list 'lambda vars bproc)) aproc))(newline)
    (analyze (append (list (list 'lambda vars bproc)) aproc))))
  

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))


(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond=>action clause) (caddr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  ;(display clauses)
  (newline)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (cond ((cond-else-clause? first)
               (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses)))
              ((equal? (cadr first) '=>)
               ;(display (cond-predicate first))
               (newline)
               ;(display (cond=>action first))
               (newline)
               ;(display (cond-predicate first))
               (newline)
               (make-if (cond-predicate first)
                        (list (cond=>action first) (cond-predicate first))
                        (expand-clauses rest)))
              (else
               (make-if (cond-predicate first)
                        (sequence->exp (cond-actions first))
                        (expand-clauses rest)))))))

;(define (last-exp? seq) (null? (cdr seq)))
;(define (first-exp seq) (car seq))
;(define (rest-exps seq) (cdr seq))
;
;(define (sequence->exp seq)
;  (cond ((null? seq) seq)
;        ((last-exp? seq) (first-exp seq))
;        (else (make-begin seq))))
;
;(define (make-begin seq) (cons 'begin seq))
;;;SECTION 4.1.3

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))


(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))


(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))


(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (undef! var env)
  (let ((frame (first-frame env)))
    (define (scan vars vals prev-vars prev-vals)
      (cond ((null? vars)
             'no-variable-to-undef)
            ((eq? var (car vars))
             (set-cdr! prev-vals (cdr vals))
             (set-cdr! prev-vars (cdr vars)))
            (else (scan (cdr vars) (cdr vals) vars vals))))
    (cond ((null? (frame-variables frame))
           'no-variable-to-undef)
          ((eq? var (car (frame-variables frame)))
           (set-car! frame (cdr (frame-variables frame)))
           (set-cdr! frame (cdr (frame-values frame))))
          (else (scan (cdr (frame-variables frame)) (cdr (frame-values frame)) (frame-variables frame) (frame-values frame))))))


;;;SECTION 4.1.4

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    (define-variable! 'import
                      (list 'primitive
			    (lambda (name)
			      (define-variable! name
				                (list 'primitive (eval name))
				                the-global-environment)))
                      initial-env)
    initial-env))

;[do later] (define the-global-environment (setup-environment))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
	(list '+ +)
	(list '- -)
	(list '* *)
	(list '/ /)
	(list '= =)
	(list 'list list)
	(list 'append append)
	(list 'equal? equal?)
	(list 'map map)
	(list 'map map)
	(list 'not not)
        (list 'memq memq)
;;      more primitives
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

;[moved to start of file] (define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
;  (display "apply ")(display proc)(newline)
;  (display "apply ")(display args)(newline)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))



(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline) (display ";;; Starting a new problem ")
            (ambeval
             input
             the-global-environment
             ;; ambeval success
             (lambda (val next-alternative)
               (announce-output output-prompt)
               (user-print val)
               (internal-loop next-alternative))
             ;; ambeval failure
             (lambda ()
               (announce-output
                ";;; There are no more values of")
               (user-print input)
               (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline) (display ";;; There is no current problem")
     (driver-loop))))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

;;;Following are commented out so as not to be evaluated when
;;; the file is loaded.
;;(define the-global-environment (setup-environment))
;;(driver-loop)

;; Added at Berkeley:
(define the-global-environment '())

(define (mce-amb)
  (set! the-global-environment (setup-environment))
  (driver-loop))


(define (ramb? exp) (tagged-list? exp 'ramb))
(define (ramb-choices exp)
  (define (remove ele-n lis)
    (define (make-new lis count)
      (display "make-new ")(display lis)(display count)(newline)
      (if (equal? count ele-n)
          (cdr lis)
          (cons (car lis) (make-new (cdr lis) (+ count 1)))))
    (make-new lis 0))
  (define (random-test exp)
    (if (null? exp)
        'skip
        (random (length exp))))
  (define (shuffle exp)
    (define rand-n (random-test exp))
    (display "rand-n ")(display rand-n)(newline)
    (display "shuffle ")(display exp)(newline)
    (if (null? exp)
        '()
        (cons (list-ref exp rand-n) (shuffle (remove rand-n exp)))))
  (shuffle (cdr exp)))

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

(define (if-fail? exp) (tagged-list? exp 'if-fail))


(define (and? exp) (tagged-list? exp 'and))

(define (and->if exp)
  (expand-and-clauses (and-clauses exp)))

(define (and-clauses exp) (cdr exp))

(define (make-and clauses)
  (append (list 'and) clauses))

(define (expand-and-clauses clauses)
;  (display clauses) (newline)
  (cond ((null? clauses)
         'true)                          ; no else clause
        ((null? (car clauses))
         'true)
        (else
         (let ((first (car clauses))
               (rest (cdr clauses)))
           (make-if first
                    (make-and rest)
                    'false)))))

(define (or? exp) (tagged-list? exp 'or))

(define (or->if exp)
  (expand-or-clauses (or-clauses exp)))

(define (or-clauses exp) (cdr exp))

(define (make-or clauses)
  (append (list 'or) clauses))

(define (expand-or-clauses clauses)
  ;(display clauses) (newline)
  (cond ((null? clauses)
         'false)                          ; no else clause
        ((null? (car clauses))
         'false)
        (else
         (let ((first (car clauses))
               (rest (cdr clauses)))
           (make-if first
                    'true
                    (make-or rest))))))







(define *unparsed* '())

(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*)) sent))

(define (parse-sentence)
  (list 'sentence (parse-noun-phrase) (parse-verb-phrase)))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend
          (list 'noun-phrase
                noun-phrase
                (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend
          (list 'verb-phrase
                verb-phrase
                (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define nouns (list 'noun 'student 'professor 'cat 'class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))
(define prepositions '(prep for to in by with))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (require p)
  (if (not p) (amb)))

;(define (generate)
;  (amb 
;  (set! *unparsed* input)
;  (let ((sent (parse-sentence)))
;    (require (null? *unparsed*)) sent))

;(if-fail (let ((x (an-element-of '(1 3 5))))
;           (require (even? x))
;           x)
;         'all-odd)