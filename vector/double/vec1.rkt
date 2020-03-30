#lang racket/base

(require glm/private/vector-ops
         glm/private/vector-types
         glm/scalar
         racket/flonum
         template)

(provide (all-defined-out) dvec1?)

(require-template glm/vector/template/vec1 define-vec1)

(define-vec1 d fl+ fl- fl* fl/ fl=)
