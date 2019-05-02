#lang racket/base
(require brag/examples/hide-and-splice
         brag/support
         rackunit)

;; check that an id with both a splice and hide is handled correctly

(check-equal? (parse-to-datum "xxx") '(top ("x" "x" "x")))
(check-equal? (parse-to-datum "yyy") '(top "y" "y" "y"))