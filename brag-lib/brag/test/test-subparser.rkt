#lang racket/base
(require brag/examples/subparser
         brag/support
         rackunit)

(check-equal? (parse-top-to-datum "x") (parse-to-datum "x"))
(check-equal? (parse-top-to-datum "x") '(top (foo (bar "x"))))
(check-equal? (parse-foo-to-datum "x") '(foo (bar "x")))
(check-equal? (parse-bar-to-datum "x") '(bar "x"))