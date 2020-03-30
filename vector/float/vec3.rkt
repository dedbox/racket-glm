#lang racket/base

(require glm/private/vector-ops
         glm/private/vector-types
         glm/scalar
         racket/flonum
         template)

(provide (all-defined-out) vec3?)

(require-template glm/vector/template/vec3 define-vec3)

(define-vec3 || fl+ fl- fl* fl/ fl=)
