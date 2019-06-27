#lang racket

(module mac racket
  (provide m)
  (println 'wasting-time)
  (define-syntax-rule (m arg) arg))

(module mod racket
  (require (submod ".." mac))
  (define (modm arg) (m arg))
  (provide modm))

(require rackunit)
(define f (let ([func-p (delay (dynamic-require '(submod "proof-of-concept.rkt" mod) 'modm))])
            (λ (x) ((force func-p) x))))

f
(f 100)
(f 100)


