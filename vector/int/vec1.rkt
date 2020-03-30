#lang racket/base

(require glm/private/vector-ops
         glm/private/vector-types
         glm/scalar
         racket/fixnum
         template)

(provide (all-defined-out) ivec1?)

(require-template glm/vector/template/vec1 define-vec1)

(define-vec1 i fx+ fx- fx* fxquotient fx=)
