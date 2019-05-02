#lang info

;; ========================================
;; pkg info

(define version "1.2")
(define collection 'multi)
(define deps '(["base" #:version "6.3"]
               "br-parser-tools-lib"
               "rackunit-lib"
               "brag-lib"))
(define build-deps '("at-exp-lib"
                     "br-parser-tools-doc"
                     "racket-doc"
                     "scribble-lib"))
(define implies '("br-parser-tools-lib"
               "brag-lib"))

;; ========================================
;; collect info

(define scribblings '(("brag.scrbl")))
(define name "brag")