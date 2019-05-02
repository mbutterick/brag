#lang racket/base
(require brag/examples/quotation-marks-and-backslashes
         brag/support
         rackunit)

(check-equal? (parse-tree "a\"'\\a\"'\\") '(start "a" "\"" "'" "\\" "a" "\"" "'" "\\"))
