#lang setup/infotab
(define name "brag")

(define scribblings '(("brag.scrbl")))
(define blurb '("brag: the Beautiful Racket AST Generator. A fork of Danny Yoo's ragg. A design goal is to be easy for beginners to use. Given a grammar in EBNF, brag produces a parser that generates Racket's native syntax objects with full source location."))
(define deps (list))
(define test-omit-paths '("examples/simple-line-drawing/examples/letter-i.rkt"))