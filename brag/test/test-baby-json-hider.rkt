#lang racket/base
(require brag/examples/baby-json-hider
         brag/support
         rackunit)

(define parse-result (parse (list "{" 
                                  (token 'ID "message")
                                  ":"
                                  (token 'STRING "'hello world'")
                                  "}")))
(check-equal? (syntax->datum parse-result) '(json (":")))

(define syntaxed-colon-parens (cadr (syntax->list parse-result)))
(check-equal? (syntax->datum (syntax-property syntaxed-colon-parens 'kvpair)) 'kvpair)

(check-equal? 
 (syntax->datum
  (parse "[[[{}]],[],[[{}]]]"))
 '(json
    (array
     "["
     (json (array "[" (json (array "[" (json) "]")) "]"))
     ","
     (json (array "[" "]"))
     ","
     (json (array "[" (json (array "[" (json) "]")) "]"))
     "]")))
