#lang racket/base
(require (for-syntax racket/base syntax/parse))

(provide #%module-begin
         ;; We reuse Racket's treatment of raw datums, specifically
         ;; for strings and numbers:
         #%datum
         
         ;; And otherwise, we provide definitions of these three forms.
         ;; During compiliation, Racket uses these definitions to 
         ;; rewrite into for loops, displays, and newlines.
         drawing rows chunk)

;; Define a few compile-time functions to do the syntax rewriting:
(begin-for-syntax
  (define (compile-drawing drawing-stx)
    (syntax-parse drawing-stx
      [({~literal drawing} row-stxs ...)

     (syntax/loc drawing-stx
       (begin row-stxs ...))]))

  (define (compile-rows row-stx)
    (syntax-parse row-stx
      [({~literal rows}
        ({~literal repeat} repeat-number)
        chunks ... 
        ";")

       (syntax/loc row-stx
         (for ([i repeat-number])
           chunks ...
           (newline)))]))

  (define (compile-chunk chunk-stx)
    (syntax-parse chunk-stx
      [({~literal chunk} chunk-size chunk-string)

       (syntax/loc chunk-stx
         (for ([k chunk-size])
           (display chunk-string)))])))


;; Wire up the use of "drawing", "rows", and "chunk" to these
;; transformers:
(define-syntax drawing compile-drawing)
(define-syntax rows compile-rows)
(define-syntax chunk compile-chunk)
