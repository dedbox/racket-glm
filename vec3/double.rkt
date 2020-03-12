#lang racket/base

(require glm/private/ops
         glm/private/types
         glm/scalar
         racket/flonum
         template
         (for-syntax racket/base))

(provide (except-out (all-defined-out) define-constructors define-vector-type))

(load-template define-constructors glm/vec3/constructors)
(load-template define-vector-type glm/vector/template)

(define-constructors d)
(define-vector-type d 3 fl+ fl- fl* fl/ fl=)
