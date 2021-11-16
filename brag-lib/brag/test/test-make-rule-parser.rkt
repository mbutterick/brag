#lang racket/base
(require rackunit
         brag/support
         brag/examples/subrule)

(define parse-next (make-rule-parser next))
(define parse-start (make-rule-parser start))

(check-equal? (syntax->datum (parse #f "0")) '(start (next "0")))
(check-equal? (syntax->datum (parse #f "0")) (syntax->datum (parse "0")))

(check-equal? (syntax->datum (parse-start #f "0")) '(start (next "0")))
(check-equal? (syntax->datum (parse-start #f "0")) (syntax->datum (parse-start "0")))

(check-equal? (syntax->datum (parse-next #f "0")) '(next "0"))
(check-equal? (syntax->datum (parse-next #f "0")) (syntax->datum (parse-next "0")))

