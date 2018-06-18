#lang racket/base
(require brag/examples/empty-symbol
         brag/support
         rackunit)

(check-true (and (member (parse-to-datum "") (list '(top (xs)) '(top (ys)) '(top (zs)))) #t))

;; x is normal
(check-equal? (parse-to-datum "x") '(top (xs "x" (xs))))
(check-equal? (parse-to-datum "xx") '(top (xs "x" (xs "x" (xs)))))
(check-equal? (parse-to-datum "xxx") '(top (xs "x" (xs "x" (xs "x" (xs))))))

;; y cuts
(check-equal? (parse-to-datum "y") '(top (ys "y")))
(check-equal? (parse-to-datum "yy") '(top (ys "y")))
(check-equal? (parse-to-datum "yyy") '(top (ys "y")))

;; z splices
(check-equal? (parse-to-datum "z") '(top (zs "z")))
(check-equal? (parse-to-datum "zz") '(top (zs "z" "z")))
(check-equal? (parse-to-datum "zzz") '(top (zs "z" "z" "z")))