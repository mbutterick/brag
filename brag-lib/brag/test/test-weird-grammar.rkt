#lang racket/base

(require "weird-grammar.rkt"
         rackunit)

(check-equal? (syntax->datum (parse '("foo")))
              '(foo "foo"))
