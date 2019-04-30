#lang racket/base
(require brag/rules/stx-types
         racket/list
         (for-syntax racket/base))
(provide flatten-rule
         flatten-rules
         prim-rule)

(define (make-fresh-name)
  (let ([n 0])
    (λ ()
      (set! n (add1 n))
      (string->symbol (format "%rule~a" n)))))

(define default-fresh-name (make-fresh-name))


;; Translates rules to lists of primitive rules.


(define (flatten-rules rules #:fresh-name [fresh-name default-fresh-name])
  (define ht (make-hasheq))
  (apply append (for/list ([a-rule (in-list rules)])
                  (flatten-rule a-rule
                                #:ht ht
                                #:fresh-name fresh-name))))


;; flatten-rule: rule -> (listof primitive-rule)
(define (flatten-rule a-rule
                      #:fresh-name [fresh-name default-fresh-name]
                      ;; ht: (hashtableof pattern-hash-key pat)
                      #:ht [ht (make-hasheq)])

  (let recur ([a-rule a-rule] [inferred? #f])

    ;; lift-nonprimitive-pattern: pattern -> (values (listof primitive-rule) pattern)
    ;; Turns non-primitive patterns into primitive patterns, and produces a set of
    ;; derived rules.
    (define (lift-nonprimitive-pattern a-pat)
      (cond
        [(primitive-pattern? a-pat)
         (values '() (linearize-primitive-pattern a-pat))]
        [(hash-has-key? ht (pattern->hash-key a-pat))
         (values '() (list (hash-ref ht (pattern->hash-key a-pat))))]
        [else
         (define head (syntax-case a-pat () [(head rest ...) #'head]))
         (define new-name (datum->syntax #f (fresh-name) a-pat))
         (define new-inferred-id (datum->syntax #f `(inferred-id  ,new-name ,head) a-pat))
         (hash-set! ht (pattern->hash-key a-pat) new-inferred-id)
         (values (recur #`(rule #,new-name #,a-pat) head)
                 (list new-inferred-id))]))

    (define (lift-nonprimitive-patterns pats)
      (define-values (rules patterns)
        (for/fold ([inferred-ruless '()]
                   [patternss '()])
                  ([p (in-list pats)])
          (define-values (new-rules new-ps)
            (lift-nonprimitive-pattern p))
          (values (cons new-rules inferred-ruless)
                  (cons new-ps patternss))))
      (values (apply append (reverse rules))
              (apply append (reverse patterns))))
          
    (with-syntax ([HEAD (if inferred? #'inferred-prim-rule #'prim-rule)]
                  [ORIGIN (syntax-case a-rule (rule) [(rule name (pat-head rest ...)) #'pat-head])])
      (syntax-case a-rule (rule)
        [(rule NAME PAT)
         (syntax-case #'PAT (id inferred-id lit token choice repeat maybe seq)

           ;; The primitive types stay as they are:
           [(id val)
            (list #'(HEAD ORIGIN NAME [PAT]))]
           [(inferred-id val reason)
            (list #'(HEAD ORIGIN NAME [PAT]))]
           [(lit val)
            (list #'(HEAD ORIGIN NAME [PAT]))]
           [(token val)
            (list #'(HEAD ORIGIN NAME [PAT]))]

           
           ;; Everything else might need lifting:
           [(choice SUB-PAT ...)
            (begin
              (define-values (inferred-ruless/rev new-sub-patss/rev)
                (for/fold ([rs '()] [ps '()])
                          ([p (syntax->list #'(SUB-PAT ...))])
                  (let-values ([(new-r new-p)
                                (lift-nonprimitive-pattern p)])
                    (values (cons new-r rs) (cons new-p ps)))))
              (with-syntax ([((SUB-PAT ...) ...) (reverse new-sub-patss/rev)])
                (append (list #'(HEAD ORIGIN NAME [SUB-PAT ...] ...))
                        (apply append (reverse inferred-ruless/rev)))))]

           [(repeat 0 0 SUB-PAT)
            ;; repeat from 0 to 0 (is a no-op)
            (recur #'(rule NAME (seq)) #f)]

           [(repeat 0 MAYBE-MAX SUB-PAT)
            ;; repeat from 0 (as a maybe rule)
            (recur #'(rule NAME (maybe (repeat 1 MAYBE-MAX SUB-PAT))) #f)]

           [(repeat MIN #f SUB-PAT)
            ;; indefinite repeat
            (begin
              (define-values (inferred-rules new-sub-pats)
                (lift-nonprimitive-pattern #'SUB-PAT))
              (with-syntax ([(SUB-PAT ...) new-sub-pats]
                            [MIN-REPEAT-SUB-PATS (apply append (make-list (syntax-e #'MIN) new-sub-pats))])
                (cons #`(HEAD ORIGIN NAME
                              [(inferred-id NAME repeat) SUB-PAT ...]
                              MIN-REPEAT-SUB-PATS)
                      inferred-rules)))]
           
           [(repeat MIN MAX SUB-PAT)
            ;; finite repeat
            (begin
              (define min (syntax-e #'MIN))
              (define max (syntax-e #'MAX))
              (unless (<= min max)
                (raise-syntax-error #f (format "minimum repeat count cannot be larger than maximum, got {~a,~a}" min max) a-rule))
              ;; has to keep the same rule NAME to work correctly
              (define new-rule-stx
                (if (= min max)
                    (with-syntax ([MIN-SUBPATS (make-list min #'SUB-PAT)])
                                  #'(rule NAME (seq . MIN-SUBPATS)))
                    (with-syntax ([REPEATS-REMAINING (- max min)]) ; REPEATS-REMAINING is a positive integer
                      #'(rule NAME (seq (repeat MIN MIN SUB-PAT) (repeat 0 REPEATS-REMAINING SUB-PAT))))))
              (recur new-rule-stx #f))]

           [(maybe SUB-PAT)
            (begin
              (define-values (inferred-rules new-sub-pats)
                (lift-nonprimitive-pattern #'SUB-PAT))
              (with-syntax ([(SUB-PAT ...) new-sub-pats])
                (cons #'(HEAD ORIGIN NAME
                              [SUB-PAT ...]
                              [])
                      inferred-rules)))]

           [(seq SUB-PAT ...)
            (begin
              (define-values (inferred-rules new-sub-pats)
                (lift-nonprimitive-patterns (syntax->list #'(SUB-PAT ...))))
              (with-syntax ([(SUB-PAT ...) new-sub-pats])
                (cons #'(HEAD ORIGIN NAME [SUB-PAT ...])
                      inferred-rules)))])]))))


;; Given a pattern, return a key appropriate for a hash.
;;
;; In the `ragg` days this used `syntax->datum` only.
;; The problem is that with cuts & splices in the mix, it creates ambiguity:
;; e.g., the pattern (/"," foo)* and ("," foo)* differ only in the 'hide syntax property
;; so `syntax->datum` does not capture their differences.
;; That means they produced the same hash key,
;; which meant they produced the same inferred pattern. Which is wrong.
;; So we adjust the key to take account of the 'hide property
;; by "lifting" it into the datum with cons.
;; Then the pattern-inference process treats them separately.
(define (pattern->hash-key a-pat)
  (let loop ([x a-pat])
    (let ([maybe-stx-list (syntax->list x)])
      (if maybe-stx-list
          (cons (syntax-property x 'hide) (map loop maybe-stx-list))
          (syntax->datum x)))))


;; Returns true if the pattern looks primitive
(define (primitive-pattern? a-pat)
  (syntax-case a-pat (id lit token choice repeat maybe seq)
    [(id val)
     #t]
    [(lit val)
     #t]
    [(token val)
     #t]
    [(choice sub-pat ...)
     #f]
    [(repeat min max val)
     #f]
    [(maybe sub-pat)
     #f]
    [(seq sub-pat ...)
     (andmap primitive-pattern? (syntax->list #'(sub-pat ...)))]))


;; Given a primitive pattern (id, lit, token, and seqs only containing
;; primitive patterns), returns a linear sequence of just id, lits,
;; and tokens.
(define (linearize-primitive-pattern a-pat)
  (define (traverse a-pat acc)
    (syntax-case a-pat (id inferred-id lit token seq)
      [(id val)
       (cons a-pat acc)]
      [(inferred-id val reason)
       (cons a-pat acc)]
      [(lit val)
       (cons a-pat acc)]
      [(token val)
       (cons a-pat acc)]
      [(seq vals ...)
       (foldl traverse acc (syntax->list #'(vals ...)))]))
  (reverse (traverse a-pat '())))



(define-syntax (prim-rule stx)
  (raise-syntax-error #f "internal error: should not be macro expanded" stx))

(define-syntax (inferred-prim-rule stx)
  (raise-syntax-error #f "internal error: should not be macro expanded" stx))

(define-syntax (inferred-id stx)
  (raise-syntax-error #f "internal error: should not be macro expanded" stx))
