#lang sicp

(#%require rackunit rackunit/text-ui "data-directed.rkt")

(run-tests 
  (test-suite
    "data directed"
    (test-case
      "single argument"

      (define (make-sum n) (attach-tag 'sum n))
      (define (make-product n) (attach-tag 'product n))

      (define (thunk x) (lambda (y . z) x))

      (put 'mzero '(sum) (thunk 0))
      (put 'mzero '(product) (thunk 1))
      (put 'mappend '(sum sum) +)
      (put 'mappend '(product product) *)

      (check-equal? 3 (apply-generic 'mappend (make-sum 1) (make-sum 2)))
      (check-equal? 2 (apply-generic 'mappend (make-product 1) (make-product 2)))

      (check-equal? 0 (apply-generic 'mzero (make-sum 100)))
      (check-equal? 1 (apply-generic 'mzero (make-product 100))))))

