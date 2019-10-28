#lang racket/base

(require ffi/unsafe
         ffi/vector
         glm/dvec
         glm/private/matrix)

(provide (all-defined-out))

(define-matrix-type dmat
  #:vector-type dvec
  #:c-type     _double
  #:ffi-vector f64vector)
