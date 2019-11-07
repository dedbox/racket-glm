#lang racket/base

(require ffi/unsafe
         ffi/vector
         glm/private/vector)

(provide (all-defined-out))

(define-numeric-vector-type vec _float real? single-flonum?
  #:from-scalar real->single-flonum
  #:to-scalar (λ (a) (real->single-flonum (if (exact? a) (exact->inexact a) a)))
  #:to-native (λ (x) (+ 0.0 x))
  #:ffi f32vector)
