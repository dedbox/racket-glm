#lang racket/base

;;; ----------------------------------------------------------------------------
;;; Clip Space Transformations
;;;
;;; glm/ext/matrix_clip_space.hpp
;;; glm/ext/matrix_clip_space.inl

(require glm/mat
         glm/mat4
         glm/trigonometric)

(provide (all-defined-out))

(define (ortho-RH-NO left right bottom top zNear zFar)
  (define m (mat4 1.0))
  (mat-set! m 0 0 (/ 2.0 (- right left)))
  (mat-set! m 1 1 (/ 2.0 (- top bottom)))
  (mat-set! m 2 2 (- (/ 2.0 (- zFar zNear))))
  (mat-set! m 0 3 (- (/ (+ right left) (- right left))))
  (mat-set! m 1 3 (- (/ (+ top bottom) (- top bottom))))
  (mat-set! m 2 3 (- (/ (+ zFar zNear) (- zFar zNear))))
  m)

(define ortho ortho-RH-NO)

(define (perspective-RH-ZO fovy aspect zNear zFar)
  (define tan-half-fovy (tan (/ fovy 2.0)))
  (define m (mat4 0.0))
  (mat-set! m 0 0 (/ 1 (* aspect tan-half-fovy)))
  (mat-set! m 1 1 (/ 1 tan-half-fovy))
  (mat-set! m 2 2 (/ zFar (- zNear zFar)))
  (mat-set! m 3 2 -1.0)
  (mat-set! m 2 3 (- (/ (* zFar zNear) (- zFar zNear))))
  m)

(define (perspective-RH-NO fovy aspect zNear zFar)
  (define tan-half-fovy (tan (/ fovy 2.0)))
  (define m (mat4 0.0))
  (mat-set! m 0 0 (/ 1 (* aspect tan-half-fovy)))
  (mat-set! m 1 1 (/ 1 tan-half-fovy))
  (mat-set! m 2 2 (- (/ (+ zFar zNear) (- zFar zNear))))
  (mat-set! m 2 3 -1.0)
  (mat-set! m 3 2 (- (/ (* 2.0 zFar zNear) (- zFar zNear))))
  m)

(define perspective perspective-RH-NO)
