#lang racket/base
(require brag/examples/nested-repeats
         rackunit)

(check-equal?
 (syntax->datum (parse (list "X" "Y" "X")))
 '(start "X" "Y" "X"))

               
