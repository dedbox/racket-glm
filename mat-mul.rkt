#lang racket/base

(require glm/factory
         glm/geometric
         glm/mat
         glm/mat4
         glm/vec
         glm/vec4
         math/flonum
         racket/function)

(provide (all-defined-out))

;;; ----------------------------------------------------------------------------

(define (mat*= m a)
  (mat=! m (mat* m a)))

(define mat*
  (case-lambda
    [(m) m]
    [(a b) ((cond [(and (mat? a) (mat? b)) mat*mat]
                  [(mat? a) (if (vec? b) mat*vec mat*scalar)]
                  [(mat? b) (if (vec? a) vec*mat scalar*mat)]
                  [else (* a b)])
            a b)]
    [(a b . cs) (apply mat* (mat* a b) cs)]))

(define (mat*mat m1 m2)
  (unless (= (mat-num-cols m1) (mat-num-rows m2))
    (error 'mat* "expected a matrix with ~a rows" (mat-num-cols m1)))
  (if (and (mat4? m1) (mat4? m2))
      (mat4*mat4 m1 m2)
      (apply (get-mat-constructor (mat-num-rows m1) (mat-num-cols m2))
             (for/list ([vk (in-mat-columns m2)])
               (mat*vec m1 vk)))))

(define (mat4*mat4 m1 m2)
  (apply mat4 (for/list ([v (in-mat-rows m2)]) (mat* v m1))))

(define (mat*vec m v)
  (unless ((mat-col-predicate m) v)
    (error 'mat* "expected vec~a" (mat-num-rows m)))
  (if (mat4? m)
      (mat4*vec m v)
      (apply (mat-row-constructor m)
             (for/list ([mk (in-mat-columns m)])
               (flsum (vec->list (vec* mk v)))))))

(define (mat4*vec m v)
  (define-values (m0 m1 m2 m3) (apply values (mat-columns m)))
  (define-values (v0 v1 v2 v3) (apply values (vec->list v)))
  (define Mov0 (vec4 v0))
  (define Mov1 (vec4 v1))
  (define Mul0 (vec* m0 Mov0))
  (define Mul1 (vec* m1 Mov1))
  (define Add0 (vec+ Mul0 Mul1))
  (define Mov2 (vec4 v2))
  (define Mov3 (vec4 v3))
  (define Mul2 (vec* m2 Mov2))
  (define Mul3 (vec* m3 Mov3))
  (define Add1 (vec+ Mul2 Mul3))
  (define Add2 (vec+ Add0 Add1))
  Add2)

(define (vec*mat v m)
  (unless ((mat-row-predicate m) v)
    (error 'mat* "expected vec~a" (mat-num-cols m)))
  (if (mat4? m)
      (vec*mat4 v m)
      (apply (mat-col-constructor m)
             (for/list ([mk (in-mat-rows m)])
               (flsum (vec->list (vec* mk v)))))))

(define (vec*mat4 v m)
  (define-values (m00 m01 m02 m03
                  m10 m11 m12 m13
                  m20 m21 m22 m23
                  m30 m31 m32 m33) (apply values (mat->list m)))
  (define-values (v0 v1 v2 v3) (apply values (vec->list v)))
  (vec4 (+ (* m00 v0) (* m01 v1) (* m02 v2) (* m03 v3))
        (+ (* m10 v0) (* m11 v1) (* m12 v2) (* m13 v3))
        (+ (* m20 v0) (* m21 v1) (* m22 v2) (* m23 v3))
        (+ (* m30 v0) (* m31 v1) (* m32 v2) (* m33 v3))))

(define (mat*scalar m x)
  (apply (mat-constructor m) (for/list ([v (in-mat-columns m)]) (vec* v x))))

(define (scalar*mat x m)
  (mat*scalar m x))

;;; ----------------------------------------------------------------------------

(module+ test
  (require glm/mat4
           glm/vec4
           racket/function
           rackunit)

  ;; (define m (mat4 0.0 0.1 0.2 0.3
  ;;                 1.0 1.1 1.2 1.3
  ;;                 2.0 2.1 2.2 2.3
  ;;                 3.0 3.1 3.2 3.3))

  ;; (define m^t (mat4 0.0 1.0 2.0 3.0
  ;;                   0.1 1.1 2.1 3.1
  ;;                   0.2 1.2 2.2 3.2
  ;;                   0.3 1.3 2.3 3.3))

  ;; (define v (vec4 4.0 5.0 6.0 7.0))

  ;; (mat* m^t v)
  ;; (mat* v m^t)

  (test-case "mat*="
    (define m (mat4 one4x4))
    (check equal? m one4x4) (mat*= m one4x4)
    (check equal? m four4x4) (mat*= m one4x4)
    (check equal? m (mat4 (vec4 16.0))) (mat*= m one4x4)
    (check equal? m (mat4 (vec4 64.0))))

  (test-case "mat*"
    (check equal? zero4x4 (mat* zero4x4))
    (check equal? zero4x4 (mat* zero4x4 0.0))
    (check equal? zero4 (mat* zero4x4 zero4))
    (check equal? zero4x4 (mat* zero4x4 zero4x4))
    (check equal? four4x4 (mat* two4x4 2.0))
    (check equal? (mat* two4x4 three4) (vec4 24.0))
    (check equal? (mat* two4x4 three4x4) (mat4 (vec4 24.0)))
    (check equal? (mat* one4x4 2.0 three4) (vec4 24.0))
    (define m (mat4  1.0  2.0  3.0  4.0
                     5.0  6.0  7.0  8.0
                     9.0 10.0 11.0 12.0
                     13.0 14.0 15.0 16.0))
    (check equal? (mat* m (vec4 1.0 0.0 0.0 0.0)) (vec4 1.0 2.0 3.0 4.0))
    (check equal? (mat* m (vec4 0.0 1.0 0.0 0.0)) (vec4 5.0 6.0 7.0 8.0))
    (check equal? (mat* m (vec4 0.0 0.0 1.0 0.0)) (vec4 9.0 10.0 11.0 12.0))
    (check equal? (mat* m (vec4 0.0 0.0 0.0 1.0)) (vec4 13.0 14.0 15.0 16.0))
    (check equal? (mat* (vec4 1.0 0.0 0.0 0.0) m) (vec4 1.0 5.0  9.0 13.0))
    (check equal? (mat* (vec4 0.0 1.0 0.0 0.0) m) (vec4 2.0 6.0 10.0 14.0))
    (check equal? (mat* (vec4 0.0 0.0 1.0 0.0) m) (vec4 3.0 7.0 11.0 15.0))
    (check equal? (mat* (vec4 0.0 0.0 0.0 1.0) m) (vec4 4.0 8.0 12.0 16.0))
    ;; (set! m (mat4 0.0 0.1 0.2 0.3
    ;;               1.0 1.1 1.2 1.3
    ;;               2.0 2.1 2.2 2.3
    ;;               3.0 3.1 3.2 3.3))
    ;; (mat* m m)
    ))
