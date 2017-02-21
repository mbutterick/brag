#lang info

(define collection 'multi)
(define deps '("base"))
(define build-deps '("scheme-lib"
                     "racket-doc"
                     "syntax-color-doc"
                     "br-parser-tools-lib"
                     "scribble-lib"))
(define update-implies '("br-parser-tools-lib"))

(define pkg-desc "documentation part of \"br-parser-tools\"")

(define pkg-authors '(mflatt))
