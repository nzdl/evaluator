#lang sicp

(#%require "table.rkt")
(#%provide put get apply-generic attach-tag)

(define all-operations (make-table))

(define (put op type proc)
    (insert! op type proc all-operations))

(define (get op type)
    (lookup op type all-operations))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (apply-generic op arg)
  (let ((tag (type-tag arg)))
    (let ((proc (get op tag)))
      (if proc
          (proc (contents arg))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op tag))))))
