#lang racket/base

(require glm/private/vector-ops
         glm/private/vector-types
         glm/scalar
         racket/flonum
         template)

(provide (all-defined-out) vec4?)

(require-template glm/vector/template/vec4 define-vec4)

(define-vec4 || fl+ fl- fl* fl/ fl=)
