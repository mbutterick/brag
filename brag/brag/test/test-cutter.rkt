#lang racket/base
(require brag/examples/cutter
         brag/support
         rackunit)

;; related to rule-flattening problem
(check-equal?
 (parse-to-datum (list "(" "x" "," "x" ")"))
 '(top (expr (list "(" (expr "x") "," (expr "x") ")"))))