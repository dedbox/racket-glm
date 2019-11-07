#lang racket/base

(require ffi/unsafe
         ffi/vector
         glm/private/matrix
         glm/vec)

(provide (all-defined-out))

(define-matrix-type mat
  #:vector-type vec
  #:c-type     _float
  #:ffi-vector f32vector)
