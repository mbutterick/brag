#lang racket/base
(require brag/examples/baby-json
         (prefix-in alt: brag/examples/baby-json-alt)
         brag/support
         rackunit)

(let ([str (list "{" 
                 (token 'ID "message")
                 ":"
                 (token 'STRING "'hello world'")
                 "}")]
      [result '(json (object "{"
                             (kvpair "message" ":" (json (string "'hello world'")))
                             "}"))])
  (check-equal? (parse-to-datum str) result)
  (check-equal? (alt:parse-to-datum str) result))


(let ([str "[[[{}]],[],[[{}]]]"]
      [result  '(json
                 (array
                  "["
                  (json (array "[" (json (array "[" (json (object "{" "}")) "]")) "]"))
                  ","
                  (json (array "[" "]"))
                  ","
                  (json (array "[" (json (array "[" (json (object "{" "}")) "]")) "]"))
                  "]"))])
  (check-equal? (parse-to-datum str) result)
  (check-equal? (alt:parse-to-datum str) result))