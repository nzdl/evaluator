#lang sicp

(#%require rackunit
           rackunit/text-ui
           "primitives.rkt"
           "environment.rkt"
           "lib.rkt"
           "../data-directed/data-directed.rkt")
(#%provide eval#
           setup-environment
           compound-procedure?
           procedure-parameters 
           procedure-body)

(define (eval# exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((get 'eval (car exp)) => (lambda (f) (f exp env)))
        ((application? exp)
         (apply# (eval# (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (apply# procedure arguments)
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
      (let ((first (eval# (first-operand exps) env)))
        (cons first
              (list-of-values (rest-operands exps) env)))))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval# (first-exp exps) env))
        (else (eval# (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval# (assignment-value exp) env)
                       env)
  'ok)

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

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

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(put 'eval 'set! eval-assignment)
(put 'eval 'lambda (lambda (exp env) (make-procedure (lambda-parameters exp) (lambda-body exp) env)))
(put 'eval 'begin (lambda (exp env) (eval-sequence (begin-actions exp) env)))

(define (install-quotation-package)
  (define (text-of-quotation exp)
    (cadr exp))
  (define (eval-quotation exp env)
    (text-of-quotation exp))
  (put 'eval 'quote eval-quotation))

(define (install-if-package)
  (define (if-predicate exp) (cadr exp))
  (define (if-consequent exp) (caddr exp))
  (define (if-alternative exp)
    (if (not (null? (cdddr exp)))
        (cadddr exp)
        'false))
  (define (eval-if exp env)
    (if (true? (eval# (if-predicate exp) env))
        (eval# (if-consequent exp) env)
        (eval# (if-alternative exp) env)))
  (put 'eval 'if eval-if))

(define (install-cond-package)
  (define (cond-clauses exp) (cdr exp))
  (define (cond-else-clause? clause)
    (eq? (cond-predicate clause) 'else))
  (define (cond-predicate clause) (car clause))
  (define (cond-actions clause) (cdr clause))
  (define (expand-clauses clauses)
    (if (null? clauses)
        'false                          ; no else clause
        (let ((first (car clauses))
              (rest (cdr clauses)))
          (if (cond-else-clause? first)
              (if (null? rest)
                  (sequence->exp (cond-actions first))
                  (error "ELSE clause isn't last -- COND->IF"
                        clauses))
              (make-if (cond-predicate first)
                      (sequence->exp (cond-actions first))
                      (expand-clauses rest))))))
  (define (cond->if exp env)
    (expand-clauses (cond-clauses exp)))
  (put 'eval 'cond cond->if))

(define (install-definition-package)
  (define (eval-definition exp env)
    (define-variable! (definition-variable exp)
      (eval# (definition-value exp) env)
      env)
    'ok)
  (put 'eval 'define eval-definition))

(install-quotation-package)
(install-if-package)
(install-cond-package)
(install-definition-package)