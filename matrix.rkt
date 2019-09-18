#lang racket/base

;;; ----------------------------------------------------------------------------
;;; Matrix Functions

(require glm/factory
         glm/mat
         glm/mat-mul
         glm/mat4
         glm/vec
         glm/vec4)

(provide (all-defined-out))

(define (transpose m)
  (define m^t ((get-mat-constructor (mat-num-cols m) (mat-num-rows m))))
  (for* ([i (in-range (mat-num-rows m^t))]
         [j (in-range (mat-num-cols m^t))])
    (mat-set! m^t i j (mat-ref m j i)))
  m^t)

(define (inverse m)
  (define-values (m00 m01 m02 m03
                  m10 m11 m12 m13
                  m20 m21 m22 m23
                  m30 m31 m32 m33) (apply values (mat->list m)))

  (let ([Coef00 (- (* m22 m33) (* m32 m23))]
        [Coef02 (- (* m12 m33) (* m32 m13))]
        [Coef03 (- (* m12 m23) (* m22 m13))]

        [Coef04 (- (* m21 m33) (* m31 m23))]
        [Coef06 (- (* m11 m33) (* m31 m13))]
        [Coef07 (- (* m11 m23) (* m21 m13))]

        [Coef08 (- (* m21 m32) (* m31 m22))]
        [Coef10 (- (* m11 m32) (* m31 m12))]
        [Coef11 (- (* m11 m22) (* m21 m12))]

        [Coef12 (- (* m20 m33) (* m30 m23))]
        [Coef14 (- (* m10 m33) (* m30 m13))]
        [Coef15 (- (* m10 m23) (* m20 m13))]

        [Coef16 (- (* m20 m32) (* m30 m22))]
        [Coef18 (- (* m10 m32) (* m30 m12))]
        [Coef19 (- (* m10 m22) (* m20 m12))]

        [Coef20 (- (* m20 m31) (* m30 m21))]
        [Coef22 (- (* m10 m31) (* m30 m11))]
        [Coef23 (- (* m10 m21) (* m20 m11))])
    (let ([Fac0 (vec4 Coef00 Coef00 Coef02 Coef03)]
          [Fac1 (vec4 Coef04 Coef04 Coef06 Coef07)]
          [Fac2 (vec4 Coef08 Coef08 Coef10 Coef11)]
          [Fac3 (vec4 Coef12 Coef12 Coef14 Coef15)]
          [Fac4 (vec4 Coef16 Coef16 Coef18 Coef19)]
          [Fac5 (vec4 Coef20 Coef20 Coef22 Coef23)])
      (let ([Vec0 (vec4 m10 m00 m00 m00)]
            [Vec1 (vec4 m11 m01 m01 m01)]
            [Vec2 (vec4 m12 m02 m02 m02)]
            [Vec3 (vec4 m13 m03 m03 m03)])
        (let ([Inv0 (vec+ (vec- (vec* Vec1 Fac0) (vec* Vec2 Fac1)) (vec* Vec3 Fac2))]
              [Inv1 (vec+ (vec- (vec* Vec0 Fac0) (vec* Vec2 Fac3)) (vec* Vec3 Fac4))]
              [Inv2 (vec+ (vec- (vec* Vec0 Fac1) (vec* Vec1 Fac3)) (vec* Vec3 Fac5))]
              [Inv3 (vec+ (vec- (vec* Vec0 Fac2) (vec* Vec1 Fac4)) (vec* Vec2 Fac5))])
          (let ([SignA (vec4 +1.0 -1.0 +1.0 -1.0)]
                [SignB (vec4 -1.0 +1.0 -1.0 +1.0)])
            (let ([Inverse (mat4 (vec* Inv0 SignA)
                                 (vec* Inv1 SignB)
                                 (vec* Inv2 SignA)
                                 (vec* Inv3 SignB))])
              (define Row0 (apply vec4 (for/list ([v (in-mat-columns Inverse)])
                                         (vec-ref v 0))))
              (define-values (Dot0.x Dot0.y Dot0.z Dot0.w)
                (apply values (vec->list (vec* (car (mat-columns m)) Row0))))
              (define OneOverDeterminant (/ 1.0 (+ (+ Dot0.x Dot0.y) (+ Dot0.z Dot0.w))))
              (mat* Inverse OneOverDeterminant))))))))
