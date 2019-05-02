#lang racket/base
(require brag/examples/curly-quantifier
         brag/support
         rackunit)

(check-exn exn:fail:parsing? (λ () (parse-to-datum "a")))
(check-equal? (parse-to-datum "aa") '(start (a-rule "a" "a")))
(check-exn exn:fail:parsing? (λ () (parse-to-datum "aaa")))

(check-equal? (parse-to-datum "") '(start (b-rule)))
(check-equal? (parse-to-datum "b") '(start (b-rule "b")))
(check-equal? (parse-to-datum "bb") '(start (b-rule "b" "b")))
(check-exn exn:fail:parsing? (λ () (parse-to-datum "bbb")))

(check-exn exn:fail:parsing? (λ () (parse-to-datum "c")))
(check-equal? (parse-to-datum "cc") '(start (c-rule "c" "c")))
(check-equal? (parse-to-datum "ccc") '(start (c-rule "c" "c" "c")))
(check-equal? (parse-to-datum "cccc") '(start (c-rule "c" "c" "c" "c")))

(check-exn exn:fail:parsing? (λ () (parse-to-datum "d")))
(check-equal? (parse-to-datum "dd") '(start (d-rule "d" "d")))
(check-equal? (parse-to-datum "ddd") '(start (d-rule "d" "d" "d")))
(check-exn exn:fail:parsing? (λ () (parse-to-datum "dddd")))

(check-equal? (syntax->datum ((make-rule-parser e-rule) "")) '(e-rule)) ; to prevent ambiguity with b-rule while parsing empty string
(check-equal? (parse-to-datum "e") '(start (e-rule "e")))
(check-equal? (parse-to-datum "ee") '(start (e-rule "e" "e")))
(check-equal? (parse-to-datum "eee") '(start (e-rule "e" "e" "e")))