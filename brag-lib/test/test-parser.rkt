#lang racket/base


(require rackunit
         br-parser-tools/lex
         brag/rules/parser
         brag/rules/lexer
         brag/rules/rule-structs)


;; quick-and-dirty helper for pos construction.
(define (p x)
  (pos x #f #f))



;; FIXME: fix the test cases so they work on locations rather than just offsets.
(check-equal? (grammar-parser (tokenize (open-input-string "expr : 'hello'")))
              (list (rule (p 1) (p 15)
                          (lhs-id (p 1) (p 5) "expr" #f)
                          (pattern-lit (p 8) (p 15) "hello" #f))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : COLON")))
              (list (rule (p 1) (p 13)
                          (lhs-id (p 1) (p 5) "expr" #f)
                          (pattern-token (p 8) (p 13) "COLON" #f))))

(check-equal? (grammar-parser (tokenize (open-input-string "/expr : COLON")))
              (list (rule (p 1) (p 14)
                          (lhs-id (p 1) (p 6) "expr" ''hide)
                          (pattern-token (p 9) (p 14) "COLON" #f))))

(check-equal? (grammar-parser (tokenize (open-input-string "@expr : COLON")))
              (list (rule (p 1) (p 14)
                          (lhs-id (p 1) (p 6) "expr" ''splice)
                          (pattern-token (p 9) (p 14) "COLON" #f))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : /COLON COLON")))
              (list (rule (p 1) (p 20)
                          (lhs-id (p 1) (p 5) "expr" #f)
                          (pattern-seq (p 8) (p 20)
                                       (list
                                        (pattern-token (p 8) (p 14) "COLON" 'hide)
                                        (pattern-token (p 15) (p 20) "COLON" #f))
                                       #f))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : /thing COLON")))
              (list (rule (p 1) (p 20)
                          (lhs-id (p 1) (p 5) "expr" #f)
                          (pattern-seq (p 8) (p 20)
                                       (list
                                        (pattern-id (p 8) (p 14) "thing" 'hide)
                                        (pattern-token (p 15) (p 20) "COLON" #f))
                                       #f))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : @thing COLON")))
              (list (rule (p 1) (p 20)
                          (lhs-id (p 1) (p 5) "expr" #f)
                          (pattern-seq (p 8) (p 20)
                                       (list
                                        (pattern-id (p 8) (p 14) "thing" 'splice)
                                        (pattern-token (p 15) (p 20) "COLON" #f))
                                       #f))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : 'hello'*")))
              (list (rule (p 1) (p 16)
                          (lhs-id (p 1) (p 5) "expr" #f)
                          (pattern-repeat (p 8) (p 16)
                                          0 #f
                                          (pattern-lit (p 8) (p 15) "hello" #f)
                                          #f))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : 'hello'+")))
              (list (rule (p 1) (p 16)
                          (lhs-id (p 1) (p 5) "expr" #f)
                          (pattern-repeat (p 8) (p 16)
                                          1 #f
                                          (pattern-lit (p 8) (p 15) "hello" #f)
                                          #f))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : [/'hello']")))
              (list (rule (p 1) (p 18)
                          (lhs-id (p 1) (p 5) "expr" #f)
                          #;(pattern-maybe (p 8) (p 18)
                                           (pattern-lit (p 9) (p 17) "hello" 'hide))
                          (pattern-repeat (p 8) (p 18)
                                          0 1
                                          (pattern-lit (p 9) (p 17) "hello" 'hide)
                                          #f))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : COLON | BLAH")))
              (list (rule (p 1) (p 20)
                          (lhs-id (p 1) (p 5) "expr" #f)
                          (pattern-choice (p 8) (p 20)
                                          (list (pattern-token (p 8) (p 13) "COLON" #f)
                                                (pattern-token (p 16) (p 20) "BLAH" #f))
                                          #f))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : COLON | BLAH | BAZ expr")))
              (list (rule (p 1) (p 31)
                          (lhs-id (p 1) (p 5) "expr" #f)
                          (pattern-choice (p 8) (p 31)
                                          (list (pattern-token (p 8) (p 13) "COLON" #f)
                                                (pattern-token (p 16) (p 20) "BLAH" #f)
                                                (pattern-seq (p 23) (p 31)
                                                             (list (pattern-token (p 23) (p 26) "BAZ" #f)
                                                                   (pattern-id (p 27) (p 31) "expr" #f))
                                                             #f))
                                          #f))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : one two /three")))
              (list (rule (p 1) (p 22)
                          (lhs-id (p 1) (p 5) "expr" #f)
                          (pattern-seq (p 8) (p 22)
                                       (list (pattern-id (p 8) (p 11) "one" #f)
                                             (pattern-id (p 12) (p 15) "two" #f)
                                             (pattern-id (p 16) (p 22) "three" 'hide))
                                       #f))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : (one two three)")))
              (list (rule (p 1) (p 23)
                          (lhs-id (p 1) (p 5) "expr" #f)
                          (pattern-seq (p 8) (p 23)
                                       (list (pattern-id (p 9) (p 12) "one" #f)
                                             (pattern-id (p 13) (p 16) "two" #f)
                                             (pattern-id (p 17) (p 22) "three" #f))
                                       #f))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : one two* three")))
              (list (rule (p 1) (p 22)
                          (lhs-id (p 1) (p 5) "expr" #f)
                          (pattern-seq (p 8) (p 22)
                                       (list (pattern-id (p 8) (p 11) "one" #f)
                                             (pattern-repeat (p 12) (p 16) 0 #f (pattern-id (p 12) (p 15) "two" #f) #f)
                                             (pattern-id (p 17) (p 22) "three" #f))
                                       #f))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : one two+ three")))
              (list (rule (p 1) (p 22)
                          (lhs-id (p 1) (p 5) "expr" #f)
                          (pattern-seq (p 8) (p 22)
                                       (list (pattern-id (p 8) (p 11) "one" #f)
                                             (pattern-repeat (p 12) (p 16) 1 #f (pattern-id (p 12) (p 15) "two" #f) #f)
                                             (pattern-id (p 17) (p 22) "three" #f))
                                       #f))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : (one two)+ three")))
              (list (rule (p 1) (p 24)
                          (lhs-id (p 1) (p 5) "expr" #f)
                          (pattern-seq (p 8) (p 24)
                                       (list (pattern-repeat (p 8) (p 18) 1 #f
                                                             (pattern-seq (p 8) (p 17)
                                                                          (list (pattern-id (p 9) (p 12) "one" #f)
                                                                                (pattern-id (p 13) (p 16) "two" #f))
                                                                          #f)
                                                             #f)
                                             (pattern-id (p 19) (p 24) "three" #f))
                                       #f))))


(check-equal? (grammar-parser (tokenize (open-input-string #<<EOF
statlist : stat+
stat: ID '=' expr
    | 'print' expr
EOF
                                                           )))
              (list (rule (p 1) (p 17)
                          (lhs-id (p 1) (p 9) "statlist" #f)
                          (pattern-repeat (p 12) (p 17) 1 #f (pattern-id (p 12) (p 16) "stat" #f) #f))
                    (rule (p 18) (p 54)
                          (lhs-id (p 18) (p 22) "stat" #f)
                          (pattern-choice (p 24) (p 54)
                                          (list (pattern-seq (p 24) (p 35)
                                                             (list (pattern-token (p 24) (p 26) "ID" #f)
                                                                   (pattern-lit (p 27) (p 30) "=" #f)
                                                                   (pattern-id (p 31) (p 35) "expr" #f))
                                                             #f)
                                                (pattern-seq (p 42) (p 54)
                                                             (list (pattern-lit (p 42) (p 49) "print" #f)
                                                                   (pattern-id (p 50) (p 54) "expr" #f))
                                                             #f))
                                          #f))))

