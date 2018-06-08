#lang racket/base
(require brag/examples/whitespace
         brag/support
         rackunit)

(check-equal?
 (parse-to-datum "\ty\n x\tz")
 '(start (tab "\t") (letter "y") (newline "\n") (space " ") (letter "x") (tab "\t") (letter "z")))

(check-equal?
 (parse-to-datum "\t\n \t")
 '(start (tab "\t") (newline "\n") (space " ") (tab "\t")))
