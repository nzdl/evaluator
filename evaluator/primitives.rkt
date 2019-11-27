#lang sicp

(#%provide primitive-procedure-names
           primitive-procedure-objects
           apply-in-underlying-scheme
           true
           false
           true?
           false?)

(define apply-in-underlying-scheme apply)
(define true #t)
(define false #f)

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)))
        ;;<more primitives>

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))
