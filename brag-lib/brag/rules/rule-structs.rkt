#lang racket/base
(provide (all-defined-out))


;; We keep our own position structure because br-parser-tools/lex's position
;; structure is non-transparent, hence highly resistant to unit testing.
(struct pos (offset line col)
        #:transparent)

(struct rule (start end lhs pattern)
        #:transparent)

(struct lhs-id (start end val splice)
        #:transparent)

;; A pattern can be one of the following:
(struct pattern (start end)
        #:transparent)

(struct pattern-id pattern (val hide)
        #:transparent)

;; Token structure to be defined by the user
(struct pattern-token pattern (val hide)
        #:transparent)

;; Token structure defined as the literal string to be matched.
(struct pattern-lit pattern (val hide)
        #:transparent)

(struct pattern-choice pattern (vals hide)
        #:transparent)

(struct pattern-repeat pattern (min
                                max
                                val
                                hide)
        #:transparent)

(struct pattern-seq pattern (vals hide)
        #:transparent)

