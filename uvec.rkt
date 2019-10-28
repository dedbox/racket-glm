#lang racket/base

(require ffi/unsafe
         ffi/vector
         glm/private/vector
         racket/math)

(provide (except-out (all-defined-out) current-uvec-precision))

(define-numeric-vector-type uvec _uint nonnegative-integer? exact-nonnegative-integer?
  #:from-scalar values
  #:to-scalar (λ (a) (if (inexact? a) (inexact->exact a) a))
  #:to-native (λ (x) (+ 0 x))
  #:ffi u32vector)
