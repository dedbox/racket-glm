#lang racket/base

;;; ----------------------------------------------------------------------------
;;; Common Transformations
;;;
;;; glm/ext/matrix_transform.hpp
;;; glm/ext/matrix_transform.inl

(require glm/geometric
         glm/mat
         glm/vec)

(provide (all-defined-out))

(define (translate m v)
  (define n (mat4 m))
  (define-values (m0 m1 m2 m3) (apply values (mat-columns m)))
  (define-values (v0 v1 v2) (apply values (vec->list v)))
  (mat-set-row! n 3 (vec+ (vec* m0 v0) (vec* m1 v1) (vec* m2 v2) m3))
  n)

(define (rotate m angle v)
  (define c (cos angle))
  (define s (sin angle))
  (define axis (normalize (vec3 v)))
  (define temp (vec* (- 1 c) axis))
  (define-values (axis0 axis1 axis2) (apply values (vec->list axis)))
  (define-values (temp0 temp1 temp2) (apply values (vec->list temp)))
  (define Rotate (mat4))
  (mat-set! Rotate 0 0 (+ c (* temp0 axis0)))
  (mat-set! Rotate 1 1 (+ c (* temp1 axis1)))
  (mat-set! Rotate 2 2 (+ c (* temp2 axis2)))
  (mat-set! Rotate 1 0 (+ (* temp0 axis1) (* s axis2)))
  (mat-set! Rotate 2 0 (- (* temp0 axis2) (* s axis1)))
  (mat-set! Rotate 0 1 (- (* temp1 axis0) (* s axis2)))
  (mat-set! Rotate 2 1 (+ (* temp1 axis2) (* s axis0)))
  (mat-set! Rotate 0 2 (+ (* temp2 axis0) (* s axis1)))
  (mat-set! Rotate 1 2 (- (* temp2 axis1) (* s axis0)))
  (define-values (m0 m1 m2 m3) (apply values (mat-columns m)))
  (define-values (Rotate00 Rotate10 Rotate20 Rotate30
                  Rotate01 Rotate11 Rotate21 Rotate31
                  Rotate02 Rotate12 Rotate22 Rotate32
                  Rotate03 Rotate13 Rotate23 Rotate33)
    (apply values (mat->list Rotate)))
  (mat4 (vec+ (vec* m0 Rotate00) (vec* m1 Rotate01) (vec* m2 Rotate02))
        (vec+ (vec* m0 Rotate10) (vec* m1 Rotate11) (vec* m2 Rotate12))
        (vec+ (vec* m0 Rotate20) (vec* m1 Rotate21) (vec* m2 Rotate22))
        m3))

(define (scale m v)
  (define n (mat4 m))
  (define-values (m0 m1 m2 m3) (apply values (mat-columns m)))
  (define-values (v0 v1 v2) (apply values (vec->list v)))
  (mat4 (vec* m0 v0) (vec* m1 v1) (vec* m2 v2) m3))

(define (look-at-LH eye center up)
  (define f (normalize (vec- center eye)))
  (define s (normalize (cross up f)))
  (define u (cross f s))
  (define m (mat4 1.0))
  (define-values (s.x s.y s.z) (apply values (vec->list s)))
  (define-values (u.x u.y u.z) (apply values (vec->list u)))
  (define-values (f.x f.y f.z) (apply values (vec->list f)))
  (mat-set! m 0 0 s.x) 
  (mat-set! m 0 1 s.y)
  (mat-set! m 0 2 s.z)
  (mat-set! m 1 0 u.x)
  (mat-set! m 1 1 u.y)
  (mat-set! m 1 2 u.z)
  (mat-set! m 2 0 (- f.x))
  (mat-set! m 2 1 (- f.y))
  (mat-set! m 2 2 (- f.z))
  (mat-set! m 3 0 (- (dot s eye)))
  (mat-set! m 3 1 (- (dot u eye)))
  (mat-set! m 3 2 (dot f eye))
  m)

(define (look-at-RH eye center up)
  (define f (normalize (vec- center eye)))
  (define s (normalize (cross f up)))
  (define u (cross s f))
  (define-values (s.x s.y s.z) (apply values (vec->list s)))
  (define-values (u.x u.y u.z) (apply values (vec->list u)))
  (define-values (f.x f.y f.z) (apply values (vec->list f)))
  ;; (mat4 s.x s.y s.z 0
  ;;       u.x u.y u.z 0
  ;;       (- f.x) (- f.y) (- f.z) 0
  ;;       (- (dot s eye)) (- (dot u eye)) (dot u eye) 1)

  (define m (mat4))
  (mat-set! m 0 0 s.x)
  (mat-set! m 0 1 s.y)
  (mat-set! m 0 2 s.z)
  (mat-set! m 1 0 u.x)
  (mat-set! m 1 1 u.y)
  (mat-set! m 1 2 u.z)
  (mat-set! m 2 0 (- f.x))
  (mat-set! m 2 1 (- f.y))
  (mat-set! m 2 2 (- f.z))
  (mat-set! m 0 3 (- (dot s eye)))
  (mat-set! m 1 3 (- (dot u eye)))
  (mat-set! m 2 3 (dot f eye))
  m

  )

(define look-at look-at-RH)
