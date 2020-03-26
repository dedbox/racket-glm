#lang template ($)

(require template (for-syntax racket/base))

(provide (all-defined-out))

(require-template glm/vector/template define-vector-type)

(define-vector-type $ 1)
