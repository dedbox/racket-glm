#lang racket/base

(require glm/private/vector-ops
         glm/private/vector-types
         glm/scalar
         racket/fixnum
         template)

(provide (all-defined-out) uvec2?)

(require-template glm/vector/template/vec2 define-vec2)

(define-vec2 u fx+ fx- fx* fxquotient fx=)
