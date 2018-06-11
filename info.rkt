#lang info

(define version "1.0")
(define collection 'multi)

(define deps '("base"
               "br-parser-tools-lib"
               "rackunit-lib"))
(define build-deps '("at-exp-lib"
                     "br-parser-tools-doc"
                     "racket-doc"
                     "scribble-lib"))
(define update-implies '("br-parser-tools-lib"))
