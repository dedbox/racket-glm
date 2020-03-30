#lang racket/base

(require glm/private/vector-ops
         glm/private/vector-types
         glm/scalar
         racket/flonum
         template)

(provide (all-defined-out) dvec2?)

(require-template glm/vector/template/vec2 define-vec2)

(define-vec2 d fl+ fl- fl* fl/ fl=)
