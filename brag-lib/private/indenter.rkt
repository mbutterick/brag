#lang racket/base
(require racket/class)
(provide indent-brag)

(define (line text pos)
  (send text position-line pos))

(define (previous-line text pos)
  (define this-line (line text pos))
  (and (positive? this-line) (sub1 this-line)))

(define (line-first-visible-char text line)
  (define (char text pos) (and pos (send text get-character pos)))
  (char text (for*/first ([pos (in-range (send text line-start-position line)
                                         (send text line-end-position line))]
                          [c (in-value (char text pos))]
                          #:unless (char-blank? c))
               pos)))

(define (indent-brag tbox [posn 0])
  (define prev-line (previous-line tbox posn))
  (define this-line (line tbox posn))
  (cond
    [(not prev-line) #f]
    [(eqv? (line-first-visible-char tbox this-line) #\|)
     (define start (send tbox line-start-position prev-line))
     (define end (send tbox line-end-position prev-line))
     (for*/first ([pos (in-range start end)]
                  [c (in-value (send tbox get-character pos))]
                  #:when (memv c '(#\: #\|)))
       (- pos start))]
    [else #f]))