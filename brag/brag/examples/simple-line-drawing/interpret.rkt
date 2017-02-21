#lang racket/base
(require syntax/parse)

(provide interpret-drawing)

(define (interpret-drawing drawing-stx)
  (syntax-parse drawing-stx
    [({~literal drawing} row-stxs ...)

     (for ([row-stx (syntax->list #'(row-stxs ...))])
       (interpret-row row-stx))]))


(define (interpret-row row-stx)
  (syntax-parse row-stx
    [({~literal rows}
      ({~literal repeat} repeat-number)
      chunks ... ";")

     (for ([i (syntax-e #'repeat-number)])
       (for ([chunk-stx (syntax->list #'(chunks ...))])
         (interpret-chunk chunk-stx))
       (newline))]))


(define (interpret-chunk chunk-stx)
  (syntax-parse chunk-stx
    [({~literal chunk} chunk-size chunk-string)

     (for ([k (syntax-e #'chunk-size)])
       (display (syntax-e #'chunk-string)))]))
