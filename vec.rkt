#lang racket/base

(require ffi/unsafe
         ffi/vector
         glm/private/vector)

(provide (all-defined-out))

(define-numeric-vector-type vec _float real? flonum?
  #:from-scalar real->double-flonum
  #:to-scalar (λ (a) (real->double-flonum (if (exact? a) (exact->inexact a) a)))
  #:to-native (λ (x) (+ 0.0 x))
  #:ffi f32vector)
