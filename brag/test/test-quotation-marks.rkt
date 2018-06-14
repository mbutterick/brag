#lang racket/base
(require brag/examples/quotation-marks
         brag/support
         rackunit)

(check-equal? (parse-tree "a\"'a\"'") '(start "a" "\"" "'" "a" "\"" "'"))
