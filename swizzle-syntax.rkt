#lang racket/base

(require syntax/parse)

(provide (all-defined-out))

(define-syntax-class swizzle-id
  (pattern :id #:when (regexp-match? #px"^[xyzw]{1,4}$"
                                     (symbol->string (syntax-e this-syntax))))
  (pattern :id #:when (regexp-match? #px"^[rgba]{1,4}$"
                                     (symbol->string (syntax-e this-syntax))))
  (pattern :id #:when (regexp-match? #px"^[stpq]{1,4}$"
                                     (symbol->string (syntax-e this-syntax)))))
