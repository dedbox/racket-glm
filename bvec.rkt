#lang racket/base

(require (except-in ffi/unsafe ->)
         ffi/vector
         glm/private/vector
         racket/contract
         racket/function)

(provide (except-out (all-defined-out) current-bvec-precision))

(provide (all-defined-out))

(define-vector-type bvec _int boolean? exact-integer? equal?
  #:from-scalar values
  #:to-scalar (λ (a) (if a 1 0))
  #:to-native (λ (x) (= x 1))
  #:fill #f
  #:ffi s32vector)

(define/contract bvec-and (-> bvec? bvec? ... bvec?)
  (case-lambda
    [(v) v]
    [(v1 v2 . vs)
     (apply bvec-and (for/bvec ([x1 (in-bvec v1)]
                                [x2 (in-bvec v2)])
                       (and x1 x2))
            vs)]))

(define/contract bvec-or (-> bvec? bvec? ... bvec?)
  (case-lambda
    [(v) v]
    [(v1 v2 . vs)
     (apply bvec-or (for/bvec ([x1 (in-bvec v1)]
                               [x2 (in-bvec v2)])
                      (or x1 x2))
            vs)]))

(define/contract (bvec-not v) (-> bvec? bvec?)
  (for/bvec ([x (in-bvec v)]) (not x)))
