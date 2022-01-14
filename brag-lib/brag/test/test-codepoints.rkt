#lang racket/base

(require brag/examples/codepoints
         rackunit)

(check-equal? (parse-to-datum '("\"A\\" "'c\\" "*def"))
              '(start (A "\"A\\") (c "'c\\") (def "*def")))
