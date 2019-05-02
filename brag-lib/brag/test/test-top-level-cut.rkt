#lang racket/base
(require (prefix-in 1: brag/examples/top-level-cut-1)
         (prefix-in 2: brag/examples/top-level-cut-2)
         (prefix-in 3: brag/examples/top-level-cut-3)
         brag/support
         rackunit)

(check-equal? (1:parse-to-datum "x") '((sub "x")))
(check-equal? (2:parse-to-datum "x") '(("x")))
(check-equal? (3:parse-to-datum "x") '("x"))

