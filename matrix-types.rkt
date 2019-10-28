#lang racket/base

(require glm/private/reprovide)

(reprovide glm/dmat glm/mat)

(module+ test
  (require ffi/unsafe
           glm/vector-types
           rackunit)

  ;; ---------------------------------------------------------------------------
  ;; mat2x2

  (test-case "mat2x2 test_operators"
    (define l (mat2x2 1.0))
    (define m (mat2x2 1.0))
    (define u (vec2 1.0))
    (define v (vec2 1.0))
    (define x 1.0)
    (define a (mat* m u))
    (define b (mat* v m))
    (define n (mat/ x m))
    (define o (mat/ m x))
    (define p (mat* x m))
    (define q (mat* m x))
    (check equal? m q)
    (check equal? m l))

  (test-case "mat2x2 test_inverse"
    (let* ([Matrix (mat2 1 2 3 4)]
           [Inverse (mat-inverse Matrix)]
           [Identity (mat* Matrix Inverse)])
      (check equal? (mat-column Identity 0) (vec2 1.0 0.0))
      (check equal? (mat-column Identity 1) (vec2 0.0 1.0)))
    (let* ([Matrix (mat2 1 2 3 4)]
           [Identity (mat/ Matrix Matrix)])
      (check equal? (mat-column Identity 0) (vec2 1.0 0.0))
      (check equal? (mat-column Identity 1) (vec2 0.0 1.0))))

  (test-case "mat2x2 test_ctr"
    (check equal? (mat2 1.0) (mat2 1.0)))

  (test-case "mat2x2 test_size"
    (check = (ctype-sizeof _mat2x2) 16)
    (check = (ctype-sizeof _dmat2x2) 32)
    (check = (mat-length (mat2x2)) 2)
    (check = (dmat-length (dmat2x2)) 2))

  ;; ---------------------------------------------------------------------------
  ;; mat2x3

  (test-case "mat2x3 test_operators"
    (define l (mat2x3 1.0))
    (define m (mat2x3 1.0))
    (define u (vec2 1.0))
    (define v (vec3 1.0))
    (define x 1.0)
    (define a (mat* m u))
    (define b (mat* v m))
    (define n (mat/ x m))
    (define o (mat/ m x))
    (define p (mat* x m))
    (define q (mat* m x))
    (check equal? m q)
    (check equal? m l))

  (test-case "mat2x3 test_ctr"
    (check equal? (mat2x3 (vec3 0 1 2) (vec3 3 4 5)) (mat2x3 0 1 2 3 4 5))
    (check equal? (mat2x3 1.0) (mat2x3 1.0)))

  (test-case "mat2x3 test_size"
    (check = (ctype-sizeof _mat2x3) 24)
    (check = (ctype-sizeof _dmat2x3) 48)
    (check = (mat-length (mat2x3)) 2)
    (check = (dmat-length (dmat2x3)) 2))

  ;; ---------------------------------------------------------------------------
  ;; mat2x4

  (test-case "mat2x4 test_operators"
    (define l (mat2x4 1.0))
    (define m (mat2x4 1.0))
    (define u (vec2 1.0))
    (define v (vec4 1.0))
    (define x 1.0)
    (define a (mat* m u))
    (define b (mat* v m))
    (define n (mat/ x m))
    (define o (mat/ m x))
    (define p (mat* x m))
    (define q (mat* m x))
    (check equal? m q)
    (check equal? m l))

  (test-case "mat2x4 test_ctr"
    (check equal? (mat2x4 (vec4 0 1 2 3) (vec4 4 5 6 7)) (mat2x4 0 1 2 3 4 5 6 7))
    (check equal? (mat2x4 1.0) (mat2x4 1.0)))

  (test-case "mat2x4 test_size"
    (check = (ctype-sizeof _mat2x4) 32)
    (check = (ctype-sizeof _dmat2x4) 64)
    (check = (mat-length (mat2x4)) 2)
    (check = (dmat-length (dmat2x4)) 2))

  ;; ---------------------------------------------------------------------------
  ;; mat3x2

  (test-case "mat3x2 test_operators"
    (define l (mat3x2 1.0))
    (define m (mat3x2 1.0))
    (define u (vec3 1.0))
    (define v (vec2 1.0))
    (define x 1.0)
    (define a (mat* m u))
    (define b (mat* v m))
    (define n (mat/ x m))
    (define o (mat/ m x))
    (define p (mat* x m))
    (define q (mat* m x))
    (check equal? m q)
    (check equal? m l))

  (test-case "mat3x2 test_ctr"
    (check equal? (mat3x2 (vec2 0 1) (vec2 2 3) (vec2 4 5)) (mat3x2 0 1 2 3 4 5))
    (check equal? (mat3x2 1.0) (mat3x2 1.0)))

  (test-case "mat3x2 test_size"
    (check = (ctype-sizeof _mat3x2) 24)
    (check = (ctype-sizeof _dmat3x2) 48)
    (check = (mat-length (mat3x2)) 3)
    (check = (dmat-length (dmat3x2)) 3))

  ;; ---------------------------------------------------------------------------
  ;; mat3x3

  (test-case "test_mat3x3"
    (define Mat0 (dmat3 (dvec3 0.6 0.2 0.3)
                        (dvec3 0.2 0.7 0.5)
                        (dvec3 0.3 0.5 0.7)))
    (check-= (for/sum ([x (in-dmat (dmat- (dmat* Mat0 (dmat-inverse Mat0))
                                          (dmat3 1.0)))])
               x) 0.0 0.01))

  (test-case "mat3x3 test_operators"
    (define l (mat3x3 1.0))
    (define m (mat3x3 1.0))
    (define u (vec3 1.0))
    (define v (vec3 1.0))
    (define x 1.0)
    (define a (mat* m u))
    (define b (mat* v m))
    (define n (mat/ x m))
    (define o (mat/ m x))
    (define p (mat* x m))
    (define q (mat* m x))
    (check equal? m q)
    (check equal? m l))

  (test-case "mat3x3 test_inverse"
    (let* ([Matrix (mat3 (vec3 0.6 0.2 0.3)
                         (vec3 0.2 0.7 0.5)
                         (vec3 0.3 0.5 0.7))]
           [Inverse (mat-inverse Matrix)]
           [Identity (mat* Matrix Inverse)])
      (check-= (for/sum ([x (in-vec (mat-column Identity 0))]) x) 1.0 0.01)
      (check-= (for/sum ([x (in-vec (mat-column Identity 1))]) x) 1.0 0.01)
      (check-= (for/sum ([x (in-vec (mat-column Identity 2))]) x) 1.0 0.01))
    (let* ([Matrix (mat3 (vec3 0.6 0.2 0.3)
                         (vec3 0.2 0.7 0.5)
                         (vec3 0.3 0.5 0.7))]
           [Identity (mat/ Matrix Matrix)])
      (check-= (for/sum ([x (in-vec (mat-column Identity 0))]) x) 1.0 0.01)
      (check-= (for/sum ([x (in-vec (mat-column Identity 1))]) x) 1.0 0.01)
      (check-= (for/sum ([x (in-vec (mat-column Identity 2))]) x) 1.0 0.01)))

  (test-case "mat3x3 test_ctr"
    (check equal?
           (mat3x3 (vec3 0 1 2) (vec3 3 4 5) (vec3 6 7 8))
           (mat3x3 0 1 2 3 4 5 6 7 8))
    (check equal? (mat3x3 1.0) (mat3x3 1.0)))

  (test-case "mat3x3 test_size"
    (check = (ctype-sizeof _mat3x3) 36)
    (check = (ctype-sizeof _dmat3x3) 72)
    (check = (mat-length (mat3x3)) 3)
    (check = (dmat-length (dmat3x3)) 3))

  ;; ---------------------------------------------------------------------------
  ;; mat3x4

  (test-case "mat3x4 test_operators"
    (define l (mat3x4 1.0))
    (define m (mat3x4 1.0))
    (define u (vec3 1.0))
    (define v (vec4 1.0))
    (define x 1.0)
    (define a (mat* m u))
    (define b (mat* v m))
    (define n (mat/ x m))
    (define o (mat/ m x))
    (define p (mat* x m))
    (define q (mat* m x))
    (check equal? m q)
    (check equal? m l))

  (test-case "mat3x4 test_ctr"
    (check equal?
           (mat3x4 (vec4 0 1 2 3) (vec4 4 5 6 7) (vec4 8 9 10 11))
           (mat3x4 0 1 2 3 4 5 6 7 8 9 10 11))
    (check equal? (mat3x4 1.0) (mat3x4 1.0)))

  (test-case "mat3x4 test_size"
    (check = (ctype-sizeof _mat3x4) 48)
    (check = (ctype-sizeof _dmat3x4) 96)
    (check = (mat-length (mat3x4)) 3)
    (check = (dmat-length (dmat3x4)) 3))

  ;; ---------------------------------------------------------------------------
  ;; mat4x2

  (test-case "mat4x2 test_operators"
    (define l (mat4x2 1.0))
    (define m (mat4x2 1.0))
    (define u (vec4 1.0))
    (define v (vec2 1.0))
    (define x 1.0)
    (define a (mat* m u))
    (define b (mat* v m))
    (define n (mat/ x m))
    (define o (mat/ m x))
    (define p (mat* x m))
    (define q (mat* m x))
    (check equal? m q)
    (check equal? m l))

  (test-case "mat4x2 test_ctr"
    (check equal?
           (mat4x2 (vec2 0 1) (vec2 2 3) (vec2 4 5) (vec2 6 7))
           (mat4x2 0 1 2 3 4 5 6 7))
    (check equal? (mat4x2 1.0) (mat4x2 1.0)))

  (test-case "mat4x2 test_size"
    (check = (ctype-sizeof _mat4x2) 32)
    (check = (ctype-sizeof _dmat4x2) 64)
    (check = (mat-length (mat4x2)) 4)
    (check = (dmat-length (dmat4x2)) 4))

  ;; ---------------------------------------------------------------------------
  ;; mat4x3

  (test-case "mat4x3 test_operators"
    (define l (mat4x3 1.0))
    (define m (mat4x3 1.0))
    (define u (vec4 1.0))
    (define v (vec3 1.0))
    (define x 1.0)
    (define a (mat* m u))
    (define b (mat* v m))
    (define n (mat/ x m))
    (define o (mat/ m x))
    (define p (mat* x m))
    (define q (mat* m x))
    (check equal? m q)
    (check equal? m l))

  (test-case "mat4x3 test_ctr"
    (check equal?
           (mat4x3 (vec3 0 1 2) (vec3 3 4 5) (vec3 6 7 8) (vec3 9 10 11))
           (mat4x3 0 1 2 3 4 5 6 7 8 9 10 11))
    (check equal? (mat4x3 1.0) (mat4x3 1.0)))

  (test-case "mat4x3 test_size"
    (check = (ctype-sizeof _mat4x3) 48)
    (check = (ctype-sizeof _dmat4x3) 96)
    (check = (mat-length (mat4x3)) 4)
    (check = (dmat-length (dmat4x3)) 4))

  ;; ---------------------------------------------------------------------------
  ;; mat4x3

  (test-case "mat4x4 test_operators"
    (let ([M (mat4x4 2.0)]
          [N (mat4x4 1.0)]
          [U (vec4 2.0)])
      (check equal? (mat* N 2.0) M)
      (check equal? (mat/ M 2.0) N)
      (check equal? (mat* M U) (vec4 4.0))
      (check equal? (mat/ U M) (vec4 1.0))
      (check equal? (mat* M N) (mat4x4 2.0)))
    (let ([M (dmat4x4 2.0)]
          [N (dmat4x4 1.0)]
          [U (dvec4 2.0)])
      (check equal? (dmat* N 2.0) M)
      (check equal? (dmat/ M 2.0) N)
      (check equal? (dmat* M U) (dvec4 4.0))
      (check equal? (dmat/ U M) (dvec4 1.0))
      (check equal? (dmat* M N) (dmat4x4 2.0))))

  (test-case "mat4x4 test_inverse"
    (let* ([Identity (mat4x4 1.0)]
           [Matrix (mat4x4 (vec4 0.6 0.2 0.3 0.4)
                           (vec4 0.2 0.7 0.5 0.3)
                           (vec4 0.3 0.5 0.7 0.2)
                           (vec4 0.4 0.3 0.2 0.6))]
           [Inverse (mat/ Identity Matrix)])
      (check-= (for/sum ([x (in-mat (mat- Identity (mat* Matrix Inverse)))]) x)
               0 0.001))
    (let* ([Identity (dmat4x4 1.0)]
           [Dmatrix (dmat4x4 (dvec4 0.6 0.2 0.3 0.4)
                             (dvec4 0.2 0.7 0.5 0.3)
                             (dvec4 0.3 0.5 0.7 0.2)
                             (dvec4 0.4 0.3 0.2 0.6))]
           [Inverse (dmat/ Identity Dmatrix)])
      (check-= (for/sum ([x (in-dmat (dmat- Identity (dmat* Dmatrix Inverse)))]) x)
               0 0.001)))

  (test-case "mat4x4 test_ctr"
    (define m0 (mat4x4 (vec4 0 1 2 3)
                       (vec4 4 5 6 7)
                       (vec4 8 9 10 11)
                       (vec4 12 13 14 15)))
    (define m1 (mat4x4 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
    (check = (* (array-length (mat-data m0))
                (ctype-sizeof (array-type (mat-data m0))))
           (* 4 4 4))
    (check equal? m0 m1)
    (check equal? (mat4x4 1.0) (mat4x4 1.0))
    (define m4 (mat4 1 0 0 0
                     0 1 0 0
                     0 0 1 0
                     0 0 0 1))
    (check = (mat-ref m4 0 0) 1.0)
    (check = (mat-ref m4 3 3) 1.0))

  (test-case "mat4x4 test_size"
    (check = (ctype-sizeof _mat4x4) 64)
    (check = (ctype-sizeof _dmat4x4) 128)
    (check = (mat-length (mat4x4)) 4)
    (check = (dmat-length (dmat4x4)) 4)))
