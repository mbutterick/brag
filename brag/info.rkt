#lang info

(define collection 'multi)

(define deps '(["base" #:version "6.3"]
               "brag-lib"))

(define build-deps '("at-exp-lib"
                     "br-parser-tools-doc"
                     "racket-doc"
                     "scribble-lib"))

(define implies '("brag-lib"))