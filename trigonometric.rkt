#lang racket/base

;;; ----------------------------------------------------------------------------
;;; Angle and Trigonometry Functions
;;;
;;; glm/trigonometric.hpp
;;; glm/detail/func_trigonometric.inl

(require glm/vec
         (rename-in racket/base
                    [sin scalar-sin]
                    [cos scalar-cos]
                    [tan scalar-tan]
                    [asin scalar-asin]
                    [acos scalar-acos]
                    [atan scalar-atan])
         (rename-in racket/math
                    [sinh scalar-sinh]
                    [cosh scalar-cosh]
                    [tanh scalar-tanh])
         (rename-in math/base
                    [asinh scalar-asinh]
                    [acosh scalar-acosh]
                    [atanh scalar-atanh]))

(provide (all-defined-out))

(define-syntax-rule (define-component-wise (name arg) scalar-expr ...)
  (define (name arg)
    (if (vec? arg)
        (apply (vec-constructor arg) (map name (vec->list arg)))
        (let () scalar-expr ...))))

(define-component-wise (radians deg) (* deg 0.01745329251994329576923690768489))
(define-component-wise (degrees rad) (* rad 57.295779513082320876798154814105))
(define-component-wise (sin x) (scalar-sin x))
(define-component-wise (cos x) (scalar-cos x))
(define-component-wise (tan x) (scalar-tan x))
(define-component-wise (asin x) (scalar-asin x))
(define-component-wise (acos x) (scalar-acos x))
(define-component-wise (atan x) (scalar-atan x))
(define-component-wise (sinh x) (scalar-sinh x))
(define-component-wise (cosh x) (scalar-cosh x))
(define-component-wise (tanh x) (scalar-tanh x))
(define-component-wise (asinh x) (scalar-asinh x))
(define-component-wise (acosh x) (scalar-acosh x))
(define-component-wise (atanh x) (scalar-atanh x))
