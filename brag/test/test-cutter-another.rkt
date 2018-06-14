#lang racket/base
(require brag/examples/cutter-another
         brag/support
         rackunit)

(check-equal? (parse-tree "w") '(top (w)))
(check-equal? (parse-tree "x") '(top (x)))
(check-equal? (parse-tree "yy") '(top (y)))
(check-equal? (parse-tree "z") '(top (z)))
(check-equal? (parse-tree "a") '(top (a)))
(check-equal? (parse-tree "bb") '(top (b)))
(check-equal? (parse-tree "c") '(top (c)))