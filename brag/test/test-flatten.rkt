#lang racket/base
(require brag/rules/stx-types
         brag/codegen/flatten
         rackunit)


(define (make-fresh-name)
  (let ([n 0])
    (lambda ()
      (set! n (add1 n))
      (string->symbol (format "r~a" n)))))


;; Simple literals
(check-equal? (map syntax->datum (flatten-rule #'(rule expr (lit "hello"))))
              '((prim-rule lit expr [(lit "hello")])))

(check-equal? (map syntax->datum
                   (flatten-rule #'(rule expr
                                         (seq (lit "hello")
                                              (lit "world")))))
              '((prim-rule seq expr [(lit "hello") (lit "world")])))


(check-equal? (map syntax->datum (flatten-rule #'(rule expr (token HELLO))))
              '((prim-rule token expr [(token HELLO)])))

(check-equal? (map syntax->datum (flatten-rule #'(rule expr (id rule-2))))
              '((prim-rule id expr [(id rule-2)])))


;; Sequences of primitives
(check-equal? (map syntax->datum
                   (flatten-rule #'(rule expr (seq (lit "1") (seq (lit "2") (lit "3"))))))
              '((prim-rule seq expr 
                           [(lit "1") (lit "2") (lit "3")])))

(check-equal? (map syntax->datum
                   (flatten-rule #'(rule expr (seq (seq (lit "1") (lit "2")) (lit "3")))))
              '((prim-rule seq expr 
                           [(lit "1") (lit "2") (lit "3")])))


(check-equal? (map syntax->datum
                   (flatten-rule #'(rule expr (seq (seq (lit "1")) (seq (lit "2") (lit "3"))))))
              '((prim-rule seq expr 
                           [(lit "1") (lit "2") (lit "3")])))



;; choices
(check-equal? (map syntax->datum
                   (flatten-rule #'(rule expr (choice (id rule-2) (id rule-3)))))
              '((prim-rule choice expr
                           [(id rule-2)]
                           [(id rule-3)])))

(check-equal? (map syntax->datum
                   (flatten-rule #'(rule sexp (choice (seq (lit "(") (lit ")"))
                                                      (seq)))
                                 #:fresh-name (make-fresh-name)))
              '((prim-rule choice sexp
                           [(lit "(") (lit ")")] [])))

(check-equal? (map syntax->datum
                   (flatten-rule #'(rule sexp (choice (seq (seq (lit "(") (token BLAH))
                                                           (lit ")"))
                                                      (seq)))
                                 #:fresh-name (make-fresh-name)))
              '((prim-rule choice sexp
                           [(lit "(") (token BLAH) (lit ")")] [])))




;; maybe
(check-equal? (map syntax->datum
                   (flatten-rule #'(rule expr (maybe (id rule-2)))))
              '((prim-rule choice expr ((id rule-2)) ())))
(check-equal? (map syntax->datum
                   (flatten-rule #'(rule expr (maybe (token HUH)))))
              '((prim-rule choice expr ((token HUH)) ())))
(check-equal? (map syntax->datum
                   (flatten-rule #'(rule expr (maybe (seq (lit "hello") (lit "world"))))))
              '((prim-rule choice expr ((lit "hello") (lit "world")) ())))

;; repeat
(check-equal? (map syntax->datum
                   (flatten-rule #'(rule rule-2+ (repeat 0 #f (id rule-2)))))
              '((prim-rule choice rule-2+ ((inferred-id %rule1 seq)) ())
    (inferred-prim-rule seq %rule1 ((inferred-id %rule2 repeat)))
    (inferred-prim-rule
     repeat
     %rule2
     ((inferred-id %rule2 repeat) (id rule-2))
     ((id rule-2)))))
(check-equal? (map syntax->datum
                   (flatten-rule #'(rule rule-2+ (repeat 0 #f (seq (lit "+") (id rule-2))))))
              '((prim-rule choice rule-2+ ((inferred-id %rule3 seq)) ())
    (inferred-prim-rule seq %rule3 ((inferred-id %rule4 repeat)))
    (inferred-prim-rule
     repeat
     %rule4
     ((inferred-id %rule4 repeat) (lit "+") (id rule-2))
     ((lit "+") (id rule-2)))))

(check-equal? (map syntax->datum
                   (flatten-rule #'(rule rule-2+ (repeat 1 #f (id rule-2)))))
              '((prim-rule repeat rule-2+
                           [(inferred-id rule-2+ repeat) (id rule-2)]
                           [(id rule-2)])))
(check-equal? (map syntax->datum
                   (flatten-rule #'(rule rule-2+ (repeat 1 #f (seq (lit "-") (id rule-2))))))
              '((prim-rule repeat rule-2+
                           [(inferred-id rule-2+ repeat) (lit "-") (id rule-2)]
                           [(lit "-") (id rule-2)])))






;; Mixtures

;; choice and maybe
(check-equal? (map syntax->datum
                   (flatten-rule #'(rule sexp (choice (lit "x")
                                                      (maybe (lit "y"))))
                                 #:fresh-name (make-fresh-name)))
              '((prim-rule choice sexp ((lit "x")) ((inferred-id r1 maybe)))
    (prim-rule choice r1 ((lit "y")) ())))

;; choice, maybe, repeat
(check-equal? (map syntax->datum
                   (flatten-rule #'(rule sexp (choice (lit "x")
                                                      (maybe (repeat 1 #f (lit "y")))))
                                 #:fresh-name (make-fresh-name)))
              '((prim-rule choice sexp ((lit "x")) ((inferred-id r1 maybe)))
    (prim-rule choice r1 ((inferred-id r2 seq)) ())
    (inferred-prim-rule seq r2 ((inferred-id r3 repeat)))
    (inferred-prim-rule
     repeat
     r3
     ((inferred-id r3 repeat) (lit "y"))
     ((lit "y")))))
;; choice, seq
(check-equal? (map syntax->datum
                   (flatten-rule #'(rule sexp (choice (seq (lit "x") (lit "y"))
                                                      (seq (lit "z") (lit "w"))))
                                 #:fresh-name (make-fresh-name)))
              '((prim-rule choice sexp
                           [(lit "x") (lit "y")]
                           [(lit "z") (lit "w")])))

;; maybe, choice
(check-equal? (map syntax->datum
                   (flatten-rule #'(rule sexp (maybe (choice (seq (lit "x") (lit "y"))
                                                             (seq (lit "z") (lit "w")))))
                                 #:fresh-name (make-fresh-name)))
              '((prim-rule choice sexp ((inferred-id r1 seq)) ())
    (inferred-prim-rule seq r1 ((inferred-id r2 choice)))
    (inferred-prim-rule choice r2 ((lit "x") (lit "y")) ((lit "z") (lit "w")))))


;; seq, repeat
(check-equal? (map syntax->datum
                   (flatten-rule #'(rule expr (seq (id term) (repeat 0 #f (seq (lit "+") (id term)))))
                                 #:fresh-name (make-fresh-name)))
              '((prim-rule seq expr ((id term) (inferred-id r1 repeat)))
    (prim-rule choice r1 ((inferred-id r2 seq)) ())
    (inferred-prim-rule seq r2 ((inferred-id r3 repeat)))
    (inferred-prim-rule
     repeat
     r3
     ((inferred-id r3 repeat) (lit "+") (id term))
     ((lit "+") (id term)))))


;; larger example: simple arithmetic
(check-equal? (map syntax->datum
                   (flatten-rules (syntax->list
                                   #'((rule expr (seq (id term) (repeat 0 #f (seq (lit "+") (id term)))))
                                      (rule term (seq (id factor) (repeat 0 #f (seq (lit "*") (id factor)))))
                                      (rule factor (token INT))))
                                  #:fresh-name (make-fresh-name)))
              
              '((prim-rule seq expr ((id term) (inferred-id r1 repeat)))
    (prim-rule choice r1 ((inferred-id r2 seq)) ())
    (inferred-prim-rule seq r2 ((inferred-id r3 repeat)))
    (inferred-prim-rule
     repeat
     r3
     ((inferred-id r3 repeat) (lit "+") (id term))
     ((lit "+") (id term)))
    (prim-rule seq term ((id factor) (inferred-id r4 repeat)))
    (prim-rule choice r4 ((inferred-id r5 seq)) ())
    (inferred-prim-rule seq r5 ((inferred-id r6 repeat)))
    (inferred-prim-rule
     repeat
     r6
     ((inferred-id r6 repeat) (lit "*") (id factor))
     ((lit "*") (id factor)))
    (prim-rule token factor ((token INT)))))
