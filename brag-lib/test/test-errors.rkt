#lang racket/base

(require rackunit
         (for-syntax racket/base))

;; The tests in this module make sure we produce proper error messages
;; on weird grammars.


(define-namespace-anchor anchor)
(define ns (namespace-anchor->namespace anchor))
(define (c prog)
  (parameterize ([current-namespace ns]
                 [read-accept-reader #t])
    (define ip (open-input-string prog))
    (port-count-lines! ip)
    (compile (read-syntax #f ip))))
    

;; Helper to let me quickly write compile-error checks.
(define-syntax (check-compile-error stx)
  (syntax-case stx ()
    [(_ prog expected-msg)
     (quasisyntax/loc stx
       (begin #,(syntax/loc stx
                  (check-exn (regexp (regexp-quote expected-msg))
                             (lambda ()
                               (c prog))))
              #,(syntax/loc stx
                  (check-exn exn:fail:syntax?
                             (lambda ()
                               (c prog))))))]))





;; errors with position are sensitive to length of lang line
(define lang-line "#lang brag")
                     
(check-compile-error (format "~a" lang-line)
                     "The grammar does not appear to have any rules")

(check-compile-error (format "~a\nfoo" lang-line)
                     "Error while parsing grammar near: foo [line=2, column=0, position=12]")

(check-compile-error (format "~a\nnumber : 42" lang-line)
                     "Error while parsing grammar near: 42 [line=2, column=9, position=21]")

(check-compile-error (format "~a\nnumber : 1" lang-line)
                     "Error while parsing grammar near: 1 [line=2, column=9, position=21]")



(check-compile-error "#lang brag\n x: NUMBER\nx:STRING"
                     "Rule x has a duplicate definition")

;; Check to see that missing definitions for rules also raise good syntax
;; errors:

(check-compile-error "#lang brag\nx:y"
                     "Rule y has no definition")

(check-compile-error "#lang brag\nnumber : 1flarbl"
                     "Rule 1flarbl has no definition")




(check-compile-error "#lang brag\nprogram: EOF"
                     "Token EOF is reserved and can not be used in a grammar")



;; Nontermination checks:
(check-compile-error "#lang brag\nx : x"
                     "Rule x has no finite derivation")



(check-compile-error #<<EOF
#lang brag
x : x y
y : "y"
EOF
                     "Rule x has no finite derivation")




; This should be illegal too:
(check-compile-error #<<EOF
#lang brag
a : "a" b
b : a | b 
EOF
                     "Rule a has no finite derivation")




(check-compile-error #<<EOF
#lang brag
a : [b]
b : [c]
c : c
EOF
                     "Rule c has no finite derivation")


(check-compile-error #<<EOF
#lang brag
a : [b]
b : c
c : c
EOF
                     "Rule b has no finite derivation")


(check-compile-error #<<EOF
#lang brag
a : [a]
b : [b]
c : c
EOF
                     "Rule c has no finite derivation")




(check-compile-error #<<EOF
#lang racket/base
(require brag/examples/simple-line-drawing)
(define bad-parser (make-rule-parser crunchy))
EOF
                     "Rule crunchy is not defined in the grammar"
                     )
