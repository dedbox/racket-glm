#lang racket/base

(require (for-syntax racket/base))

(provide reprovide)

(define-syntax (reprovide stx)
  (syntax-case stx ()
    [(_ mod-path ...)
     (syntax-local-introduce
      #'(begin
          (require mod-path ...)
          (provide (all-from-out mod-path ...))))]))
