#lang racket/base

(provide (all-defined-out))

(define-syntax-rule (reprovide mod ...)
  (begin (require mod ...) (provide (all-from-out mod ...))))
