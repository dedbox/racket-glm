#lang racket/base

(require glm/private/vector-ops
         glm/private/vector-types
         glm/scalar
         racket/flonum
         template)

(provide (all-defined-out) vec2?)

(require-template glm/vector/template/vec2 define-vec2)

(define-vec2 || fl+ fl- fl* fl/ fl=)
