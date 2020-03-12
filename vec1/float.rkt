#lang racket/base

(require glm/private/ops
         glm/private/types
         glm/scalar
         racket/flonum
         template
         (for-syntax racket/base))

(provide (except-out (all-defined-out) define-constructors define-vector-type))

(load-template define-constructors glm/vec1/constructors)
(load-template define-vector-type glm/vector/template)

(define-constructors ||)
(define-vector-type || 1 fl+ fl- fl* fl/ fl=)
