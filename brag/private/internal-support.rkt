#lang racket/base

(require brag/support)

(provide current-source
         current-parser-error-handler
         current-tokenizer-error-handler)

;; During parsing, we should define the source of the input.
(define current-source (make-parameter #f))


;; When an parse error happens, we call the current-parser-error-handler:
(define current-parser-error-handler
  (make-parameter
   (lambda (tok-name tok-value offset line col span)
     (raise (exn:fail:parsing
                (format "Encountered parsing error near ~e (token ~e) while parsing ~e [line=~a, column=~a, offset=~a]"
                        tok-value tok-name
                        (current-source)
                        line col offset)
                (current-continuation-marks)
                (list (srcloc (current-source) line col offset span)))))))

;; When a tokenization error happens, we call the current-tokenizer-error-handler.
(define current-tokenizer-error-handler
  (make-parameter
   (lambda (tok-type tok-value offset line column span)
     (raise (exn:fail:parsing
             (format "Encountered unexpected token ~e (~e) while parsing ~e [line=~a, column=~a, offset=~a]"
                     tok-type
                     tok-value
                     (current-source)
                     line column offset)
             (current-continuation-marks)
             (list (srcloc (current-source) line column offset span)))))))
