#lang s-exp syntax/module-reader
brag/codegen/expander
#:read my-read
#:read-syntax my-read-syntax
#:info my-get-info
#:whole-body-readers? #t

(require brag/rules/parser
         brag/rules/lexer
         brag/rules/stx
         brag/rules/rule-structs)

(define (my-read in) (syntax->datum (my-read-syntax #f in)))

(define ((my-parser-error-handler src) tok-ok? tok-name tok-value start-pos end-pos)
    (raise-syntax-error 
     #f
     (format "Error while parsing grammar near: ~a [line=~a, column=~a, position=~a]"
             tok-value
             (pos-line start-pos)
             (pos-col start-pos)
             (pos-offset start-pos))
     (datum->syntax #f
                    (string->symbol (format "~a" tok-value))
                    (list src
                          (pos-line start-pos)
                          (pos-col start-pos)
                          (pos-offset start-pos)
                          (if (and (number? (pos-offset end-pos))
                                   (number? (pos-offset start-pos)))
                              (- (pos-offset end-pos)
                                 (pos-offset start-pos))
                              #f)))))

(define (my-read-syntax src in)
  (define tokenizer (tokenize in))
  (define rules (parameterize ([current-parser-error-handler (my-parser-error-handler src)])
                  (grammar-parser tokenizer)))
  (for/list ([r (in-list rules)])
            (rule->stx src r)))

(define (my-get-info key default default-filter)
  (case key
    [(color-lexer) (dynamic-require 'brag/private/colorer 'color-brag (λ () #f))]
    [(drracket:indentation) (dynamic-require 'brag/private/indenter 'indent-brag (λ () #f))]
    [else (default-filter key default)]))

