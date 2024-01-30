#lang sicp

;1
;setup-environment

;1a mc-eval;
;mc-eval
;list-of-values
;eval-if
;eval-sequence
;eval-assignment
;eval-definition
;driver-loop

;2 apply
;apply-primitive-procedure

;2a mc-apply
;mc-eval

;3 because in scheme, procedures are not immediately applied to arguments when initialized; sort of
;wrong? Answer: none of the arguments to lambda should be evaluated. In particular, the expressions
;that make up the body of the procedure are not evaluated until the procedure is invoked.

;4.1
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((car-term (mc-eval (first-operand exps) env)) ;Not quite right because it assume left to
            (cdr-term (list-of-values (rest-operands exps) env))) ;right evaulation of let. I should
        (cons car-term cdr-term)))) ;have just had the first item I wanted evaluated in the let

#| (define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((cdr-term (list-of-values (rest-operands exps) env))
            (car-term (mc-eval (first-operand exps) env)))
        (cons car-term cdr-term)))) |#

;4.2
; (Cheated and just made the change) set! is not a defined symbol, so and since the application will


; accept a set! expression, it ends up trying to evauluta it as a symbol
;changed the following
(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))

;4.4, 4.5 completed in racket