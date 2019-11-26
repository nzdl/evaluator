#lang sicp

(#%require rackunit rackunit/text-ui "table.rkt")

(run-tests
  (test-suite
    "two demensional table"
    (test-case
      "assoc and lookup"
      (define my-table (make-table))
      (insert! 'a 'a 1 my-table)
      (insert! 'a 'b 2 my-table)
      (insert! 'b 'a 3 my-table)
      (insert! 'b 'b 4 my-table)
      (check-equal? (lookup 'a 'a my-table) 1)
      (check-equal? (lookup 'a 'b my-table) 2)
      (check-equal? (lookup 'b 'a my-table) 3)
      (check-equal? (lookup 'b 'b my-table) 4))))