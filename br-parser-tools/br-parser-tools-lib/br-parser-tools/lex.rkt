#lang racket/base

;; Provides the syntax used to create lexers and the functions needed to
;; create and use the buffer that the lexer reads from.  See docs.

(require (for-syntax mzlib/list
                     syntax/stx
                     syntax/define
                     syntax/boundmap
                     "private-lex/util.rkt"
                     "private-lex/actions.rkt"
                     "private-lex/front.rkt"
                     "private-lex/unicode-chars.rkt"
                     racket/base
                     racket/promise))

(require mzlib/stxparam
         syntax/readerr
         "private-lex/token.rkt")

(provide lexer lexer-src-pos lexer-srcloc define-lex-abbrev define-lex-abbrevs define-lex-trans
           
         ;; Dealing with tokens and related structures 
         define-tokens define-empty-tokens token-name token-value token?
         (struct-out position)
         (struct-out position-token)
         (struct-out srcloc-token)
           
         ;; File path for highlighting errors while lexing
         file-path
         lexer-file-path ;; alternate name
           
         ;; Lex abbrevs for unicode char sets.  See mzscheme manual section 3.4.
         any-char any-string nothing alphabetic lower-case upper-case title-case
         numeric symbolic punctuation graphic whitespace blank iso-control

         ;; A regular expression operator
         char-set)
  
;; wrap-action: syntax-object src-pos? -> syntax-object
(define-for-syntax (wrap-action action src-loc-style)
  (with-syntax ([action-stx 
                 (cond
                   [(eq? src-loc-style 'lexer-src-pos)
                    #`(let/ec ret
                        (syntax-parameterize 
                            ([return-without-pos (make-rename-transformer #'ret)])
                          (make-position-token #,action start-pos end-pos)))]
                   [(eq? src-loc-style 'lexer-srcloc)
                    #`(let/ec ret
                        (syntax-parameterize 
                            ([return-without-srcloc (make-rename-transformer #'ret)])
                          (make-srcloc-token #,action lexeme-srcloc)))]
                   [else action])])
    (syntax/loc action
      (λ (start-pos-p end-pos-p lexeme-p input-port-p)
        (define lexeme-srcloc-p (make-srcloc (object-name input-port-p)
                                             (position-line start-pos-p)
                                             (position-col start-pos-p)
                                             (position-offset start-pos-p)
                                             (and (number? (position-offset end-pos-p))
                                                  (number? (position-offset start-pos-p))
                                                  (- (position-offset end-pos-p)
                                                     (position-offset start-pos-p)))))
        (syntax-parameterize 
            ([start-pos (make-rename-transformer #'start-pos-p)]
             [end-pos (make-rename-transformer #'end-pos-p)]
             [lexeme (make-rename-transformer #'lexeme-p)]
             [input-port (make-rename-transformer #'input-port-p)]
             [lexeme-srcloc (make-rename-transformer #'lexeme-srcloc-p)])
          action-stx)))))
        
(define-for-syntax (make-lexer-macro caller src-loc-style)
  (λ (stx)
    (syntax-case stx ()
      [(_ . RE+ACTS)         
       (let ()
         (define spec/re-acts (syntax->list #'RE+ACTS))
         (for/and ([x (in-list spec/re-acts)])
           (syntax-case x ()
             [(RE ACT) #t]
             [else (raise-syntax-error caller "not a regular expression / action pair" stx x)]))
         (define eof-act (get-special-action spec/re-acts #'eof #'eof))
         (define spec-act (get-special-action spec/re-acts #'special #'(void)))
         (define spec-comment-act (get-special-action spec/re-acts #'special-comment #'#f))
         (define ids (list #'special #'special-comment #'eof))
         (define re-acts (filter (λ (spec/re-act)
                                   (syntax-case spec/re-act ()
                                     [((special) act)
                                      (not (ormap
                                            (λ (x)
                                              (and (identifier? #'special)
                                                   (module-or-top-identifier=? #'special x)))
                                            ids))]
                                     [_ #t])) spec/re-acts))
         (define names (map (λ (x) (datum->syntax #f (gensym))) re-acts))
         (define acts (map (λ (x) (stx-car (stx-cdr x))) re-acts))
         (define re-actnames (map (λ (re-act name) (list (stx-car re-act) name)) re-acts names))
         (when (null? spec/re-acts)
           (raise-syntax-error caller "expected at least one action" stx))
         (define-values (trans start action-names no-look disappeared-uses) (build-lexer re-actnames))
         (when (vector-ref action-names start) ;; Start state is final
           (unless (and 
                    ;; All the successor states are final
                    (vector? (vector-ref trans start))
                    (andmap (λ (x) (vector-ref action-names (vector-ref x 2)))
                            (vector->list (vector-ref trans start)))
                    ;; Each character has a successor state
                    (let loop ([check 0]
                               [nexts (vector->list (vector-ref trans start))])
                      (cond
                        [(null? nexts) #f]
                        [else
                         (let ([next (car nexts)])
                           (and (= (vector-ref next 0) check)
                                (let ([next-check (vector-ref next 1)])
                                  (or (>= next-check max-char-num)
                                      (loop (add1 next-check) (cdr nexts))))))])))
             (eprintf "warning: lexer at ~a can accept the empty string\n" stx)))
         (with-syntax ([START-STATE-STX start]
                       [TRANS-TABLE-STX trans]
                       [NO-LOOKAHEAD-STX no-look]
                       [(NAME ...) names]
                       [(ACT ...) (map (λ (a) (wrap-action a src-loc-style)) acts)]
                       [(ACT-NAME ...) (vector->list action-names)]
                       [SPEC-ACT-STX (wrap-action spec-act src-loc-style)]
                       [HAS-COMMENT-ACT?-STX (if (syntax-e spec-comment-act) #t #f)]
                       [SPEC-COMMENT-ACT-STX (wrap-action spec-comment-act src-loc-style)]
                       [EOF-ACT-STX (wrap-action eof-act src-loc-style)])
           (syntax-property
            (syntax/loc stx (let ([NAME ACT] ...)
                              (let ([proc (lexer-body START-STATE-STX 
                                                      TRANS-TABLE-STX
                                                      (vector ACT-NAME ...)
                                                      NO-LOOKAHEAD-STX
                                                      SPEC-ACT-STX
                                                      HAS-COMMENT-ACT?-STX
                                                      SPEC-COMMENT-ACT-STX
                                                      EOF-ACT-STX)])
                                ;; reverse eta to get named procedures:
                                (λ (port) (proc port)))))
            'disappeared-use disappeared-uses)))])))

(define-syntax lexer (make-lexer-macro 'lexer #f))
(define-syntax lexer-src-pos (make-lexer-macro 'lexer-src-pos 'lexer-src-pos))
(define-syntax lexer-srcloc (make-lexer-macro 'lexer-srcloc 'lexer-srcloc))
    
(define-syntax (define-lex-abbrev stx)
  (syntax-case stx ()
    [(_ NAME RE) (identifier? #'NAME)
                 (syntax/loc stx
                   (define-syntax NAME
                     (make-lex-abbrev (λ () (quote-syntax RE)))))]
    [_ (raise-syntax-error 'define-lex-abbrev "form should be (define-lex-abbrev name re)" stx)]))

(define-syntax (define-lex-abbrevs stx)
  (syntax-case stx ()
    [(_ . XS)
     (with-syntax ([(ABBREV ...) (map 
                                  (λ (a)
                                    (syntax-case a ()
                                      [(NAME RE) (identifier? #'NAME)
                                                 (syntax/loc a (define-lex-abbrev NAME RE))]
                                      [_ (raise-syntax-error
                                          #f
                                          "form should be (define-lex-abbrevs (name re) ...)"
                                          stx
                                          a)]))
                                  (syntax->list #'XS))])
       (syntax/loc stx (begin ABBREV ...)))]
    [_ (raise-syntax-error #f "form should be (define-lex-abbrevs (name re) ...)" stx)]))

(define-syntax (define-lex-trans stx)
  (syntax-case stx ()
    [(_ name-form body-form)
     (let-values (((name body)
                   (normalize-definition #'(define-syntax name-form body-form) #'λ)))
         
       #`(define-syntax #,name 
           (let ((func #,body))
             (unless (procedure? func)
               (raise-syntax-error 'define-lex-trans "expected a procedure as the transformer, got ~e" func))
             (unless (procedure-arity-includes? func 1)
               (raise-syntax-error 'define-lex-trans "expected a procedure that accepts 1 argument as the transformer, got ~e" func))
             (make-lex-trans func))))]
    [_
     (raise-syntax-error
      #f
      "form should be (define-lex-trans name transformer)"
      stx)]))
       

(define (get-next-state-helper char min max table)
  (cond
    [(>= min max) #f]
    [else
     (define try (quotient (+ min max) 2))
     (define el (vector-ref table try))
     (define r1 (vector-ref el 0))
     (define r2 (vector-ref el 1))
     (cond
       [(and (>= char r1) (<= char r2)) (vector-ref el 2)]
       [(< char r1) (get-next-state-helper char min try table)]
       [else (get-next-state-helper char (add1 try) max table)])]))
               
          
          
  
(define (get-next-state char table)
  (and table (get-next-state-helper char 0 (vector-length table) table)))
  
(define (lexer-body start-state trans-table actions no-lookahead special-action
                    has-special-comment-action? special-comment-action eof-action)
  (letrec ([lexer
            (λ (ip)
              (let ((first-pos (get-position ip))
                    (first-char (peek-char-or-special ip 0)))
                ;(printf "(peek-char-or-special port 0) = ~e\n" first-char)
                (cond
                  [(eof-object? first-char)
                   (do-match ip first-pos eof-action (read-char-or-special ip))]
                  [(special-comment? first-char)
                   (read-char-or-special ip)
                   (cond
                     (has-special-comment-action?
                      (do-match ip first-pos special-comment-action #f))
                     (else (lexer ip)))]
                  [(not (char? first-char))
                   (do-match ip first-pos special-action (read-char-or-special ip))]
                  [else
                   (let lexer-loop (
                                    ;; current-state
                                    (state start-state)
                                    ;; the character to transition on
                                    (char first-char)
                                    ;; action for the longest match seen thus far
                                    ;; including a match at the current state
                                    (longest-match-action 
                                     (vector-ref actions start-state))
                                    ;; how many bytes precede char
                                    (length-bytes 0)
                                    ;; how many characters have been read
                                    ;; including the one just read
                                    (length-chars 1)
                                    ;; how many characters are in the longest match
                                    (longest-match-length 0))
                     (let ([next-state 
                            (cond
                              [(not (char? char)) #f]
                              [else (get-next-state (char->integer char)
                                                    (vector-ref trans-table state))])])
                       (cond
                         [(not next-state)
                          (check-match ip first-pos longest-match-length
                                       length-chars longest-match-action)]
                         [(vector-ref no-lookahead next-state)
                          (let ((act (vector-ref actions next-state)))
                            (check-match ip 
                                         first-pos 
                                         (if act length-chars longest-match-length)
                                         length-chars
                                         (if act act longest-match-action)))]
                         [else
                          (let* ([act (vector-ref actions next-state)]
                                 [next-length-bytes (+ (char-utf-8-length char) length-bytes)]
                                 [next-char (peek-char-or-special ip next-length-bytes)])
                            #;(printf "(peek-char-or-special port ~e) = ~e\n"
                                      next-length-bytes next-char)
                            (lexer-loop next-state 
                                        next-char
                                        (if act
                                            act
                                            longest-match-action)
                                        next-length-bytes
                                        (add1 length-chars)
                                        (if act
                                            length-chars
                                            longest-match-length)))])))])))])
    (λ (ip)
      (unless (input-port? ip)
        (raise-argument-error 'lexer "input-port?" 0 ip))
      (lexer ip))))
      
(define (check-match lb first-pos longest-match-length length longest-match-action)
  (unless longest-match-action
    (let* ([match (read-string length lb)]
           [end-pos (get-position lb)])
      (raise-read-error
       (format "lexer: No match found in input starting with: ~a" match)
       (file-path)
       (position-line first-pos)
       (position-col first-pos)
       (position-offset first-pos)
       (- (position-offset end-pos) (position-offset first-pos)))))
  (let ([match (read-string longest-match-length lb)])
    ;(printf "(read-string ~e port) = ~e\n" longest-match-length match)
    (do-match lb first-pos longest-match-action match)))

(define file-path (make-parameter #f))
(define lexer-file-path file-path)

(define (do-match ip first-pos action value)
  #;(printf "(action ~a ~a ~a ~a)\n" 
            (position-offset first-pos) (position-offset (get-position ip)) value ip)
  (action first-pos (get-position ip) value ip))
  
(define (get-position ip)
  (define-values (line col off) (port-next-location ip))
  (make-position off line col))

(define-syntax (create-unicode-abbrevs stx)
  (syntax-case stx ()
    [(_ CTXT)
     (with-syntax ([(RANGES ...) (for/list ([range (in-list (list (force alphabetic-ranges)
                                                                  (force lower-case-ranges)
                                                                  (force upper-case-ranges)
                                                                  (force title-case-ranges)
                                                                  (force numeric-ranges)
                                                                  (force symbolic-ranges)
                                                                  (force punctuation-ranges)
                                                                  (force graphic-ranges)
                                                                  (force whitespace-ranges)
                                                                  (force blank-ranges)
                                                                  (force iso-control-ranges)))])
                                   `(union ,@(map (λ (x)
                                                    `(char-range ,(integer->char (car x))
                                                                 ,(integer->char (cdr x))))
                                                  range)))]
                   [(NAMES ...) (for/list ([sym (in-list '(alphabetic
                                                           lower-case
                                                           upper-case
                                                           title-case
                                                           numeric
                                                           symbolic
                                                           punctuation
                                                           graphic
                                                           whitespace
                                                           blank
                                                           iso-control))])
                                  (datum->syntax #'CTXT sym #f))])
       #'(define-lex-abbrevs (NAMES RANGES) ...))]))
                                             
(define-lex-abbrev any-char (char-complement (union)))
(define-lex-abbrev any-string (intersection))
(define-lex-abbrev nothing (union))
(create-unicode-abbrevs #'here)
  
(define-lex-trans (char-set stx)
  (syntax-case stx ()
    [(_ STR)
     (string? (syntax-e #'STR))
     (with-syntax ([(CHAR ...) (string->list (syntax-e #'STR))])
       #'(union CHAR ...))]))

(define-syntax provide-lex-keyword
  (syntax-rules ()
    [(_ ID ...)
     (begin
       (define-syntax-parameter ID
         (make-set!-transformer
          (λ (stx)
            (raise-syntax-error
             'provide-lex-keyword
             (format "use of a lexer keyword (~a) is not in an appropriate lexer action" 'ID)
             stx))))
       ...
       (provide ID ...))]))
  
(provide-lex-keyword start-pos end-pos lexeme lexeme-srcloc input-port return-without-pos return-without-srcloc)
