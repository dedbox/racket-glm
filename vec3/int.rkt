#lang racket/base

(require glm/private/ops
         glm/private/types
         glm/scalar
         template
         (for-syntax racket/base))

(provide (except-out (all-defined-out) define-constructors define-vector-type))

(load-template define-constructors glm/vec3/constructors)
(load-template define-vector-type glm/vector/template)

(define-constructors i)
(define-vector-type i 3 + - * / =)
