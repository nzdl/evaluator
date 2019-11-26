#lang sicp

(#%require rackunit "evaluator.rkt")

(define env (setup-environment))

(define (eval-exp exp)
  (eval1 exp env))

(define (check-eval? expression expected)
  (check-equal? (eval-exp expression) expected))

(eval-exp
  '(define (append x y)
    (if (null? x)
      y
      (cons (car x)
        (append (cdr x) y)))))

(check-eval? '(append '(a b c) '(d e f)) '(a b c d e f))
