#lang racket/base

(require glm/private/ops
         glm/private/types
         glm/scalar
         template
         (for-syntax racket/base))

(provide (except-out (all-defined-out)
                     define-constructors define-vector-type define-extra-boolean-ops))

(load-template define-constructors glm/vec2/constructors)
(load-template define-vector-type glm/vector/template)
(load-template define-extra-boolean-ops glm/vector/template/bool)

(define-constructors b)
(define-vector-type b 2 + - * / =)
(define-extra-boolean-ops 2)
