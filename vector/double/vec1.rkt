#lang racket/base

(require glm/private/vector-ops glm/scalar racket/flonum template)

(provide (all-defined-out))

(require-template glm/vector/template/vec1 define-vec1)

(define-vec1 d fl+ fl- fl* fl/ fl=)
