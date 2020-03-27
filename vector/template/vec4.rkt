#lang template ($)

(require glm/private/vector-types
         template (for-syntax racket/base))

(provide (all-defined-out))

(require-template glm/vector/template define-vector-type)

(define-vector-type $ 4)
