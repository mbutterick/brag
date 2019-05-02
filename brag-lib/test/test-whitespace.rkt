#lang racket/base
(require brag/examples/whitespace
         brag/support
         rackunit)

(check-equal?
 (parse-to-datum "\ty\n x\tz\r")
 '(start (tab "\t") (letter "y") (newline "\n") (space " ") (letter "x") (tab "\t") (letter "z") (return "\r")))

(check-equal?
 (parse-to-datum "\t\n \t\r")
 '(start (tab "\t") (newline "\n") (space " ") (tab "\t") (return "\r")))

(check-equal?
 (parse-to-datum "\a\b\t\n\v\f\r\e")
 '(start (all "\a" "\b" "\t" "\n" "\v" "\f" "\r" "\e")))
