#lang racket/base

(require glm/private/vector-ops
         glm/private/vector-types
         glm/scalar
         racket/fixnum
         template)

(provide (all-defined-out) uvec3?)

(require-template glm/vector/template/vec3 define-vec3)

(define-vec3 u fx+ fx- fx* fxquotient fx=)
