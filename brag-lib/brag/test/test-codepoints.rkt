#lang racket/base

(require brag/examples/codepoints
         rackunit)

(check-equal? (parse-to-datum "Acde") '(start (A "A") (c "c") (d "d") (e "e")))
