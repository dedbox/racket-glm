#lang template ($ ⊕ ⊖ ⊗ ⊘ ≗)

(require glm/private/vector-types template (for-syntax racket/base))

(require-template glm/vector/template define-vector-type)

(define-vector-type $ 2 ⊕ ⊖ ⊗ ⊘ ≗)
