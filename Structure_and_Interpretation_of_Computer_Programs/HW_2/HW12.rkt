;4.3
;completely forgot about the whole evaluation thing for special forms
(define (mc-eval exp env)
  (display exp)
  (cond ((self-evaluating? exp) (display "self-eval") (newline) exp)
	    ((variable? exp) (display "var") (newline) (lookup-variable-value exp env))
	    (else ((get 'mc-eval (exp-type exp)) (rest-of-exp exp) ))))
(define (exp-type exp)
  (car exp))
(define (rest-of-exp exp)
  (cdr exp))

;take 2
(define (mc-eval exp env)
  (display exp)
  (cond ((self-evaluating? exp) (display "self-eval") (newline) exp)
	    ((variable? exp) (display "var") (newline) (lookup-variable-value exp env))
	    (else (let ((special-form (get 'mc-eval (exp-type exp))))
                (if (special-form exp env)
                    (special-form exp env)
                    (mc-apply (mc-eval (operator exp) env)
                              (list-of-values (operands exp) env)))))))

(define (exp-type exp)
  (car exp))
(put 'mc-eval 'and (lambda (exp) (mc-eval (and->if exp) env)))
...etc for all special-forms


;4.6

(define (let? exp) (tagged-list? exp 'let))

(define (let-params exp) (map car (cadr exp)))
(define (let-args exp) (map cadr (cadr exp)))
(define (let-body exp) (caddr exp))
(define (let->lambda exp)
  (append (list (list 'lambda (let-params exp) (let-body exp))) (let-args exp))) 





;, 4.7*, 4.10*, 4.11*, skipped

;4.13
;Completed in racket I went with only unbinding the variable in the current frame to mimic how define
;adds bindings



;4.14
;Not sure, I guess it may have to do with how map also calls apply? Or that it is passing a meta-list
;into the scheme argument that expects a scheme list? Close - it is about the meta procedure
;passed to the scheme map.


;4.15
;if (t t) halts, then the first clasue would have actiuvated and (t t) woul dhave gone on forever,
;which violate our assumtion that (t t) halted.
;if (t t) ran for ever, the (t t) would have halted, which also can't have happened. There for it is
;impossible for the halting procedure to be correct.