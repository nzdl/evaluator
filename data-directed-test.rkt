#lang sicp

(#%require rackunit rackunit/text-ui "data-directed.rkt")

(run-tests 
    (test-suite
        "data directed"
        (test-case
            "single argument"

            (define (make-sum n) (attach-tag 'sum n))
            (define (make-product n) (attach-tag 'product n))

            (put 'mappend 'sum +)
            (put 'mappend 'product *)
            (put 'mzero 'sum (lambda (x) 0))
            (put 'mzero 'product (lambda (x) 1))

            (check-equal? 0 (apply-generic 'mzero (make-sum 1)))
            (check-equal? 1 (apply-generic 'mzero (make-product 1)))
            )))
