#lang racket/base

(require glm/private/vector-ops
         glm/private/vector-types
         glm/scalar
         racket/fixnum
         template)

(provide (all-defined-out) uvec1?)

(require-template glm/vector/template/vec1 define-vec1)

(define-vec1 u fx+ fx- fx* fxquotient fx=)
