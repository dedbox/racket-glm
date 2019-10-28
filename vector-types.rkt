#lang racket/base

(require glm/private/reprovide)

(reprovide glm/bvec glm/dvec glm/ivec glm/uvec glm/vec)

(module+ test
  (require rackunit
           (for-syntax racket/base))

  ;; ---------------------------------------------------------------------------

  (test-case "test_vec1_operators"
    (define A (ivec1 1))
    (define B (ivec1 1))
    (check equal? A B)
    (ivec*=! A 1)
    (ivec*=! B 1)
    (ivec+=! A 1)
    (ivec+=! B 1)
    (check equal? A B))

  (test-case "test_vec1_size"
    (check = (vec-length (vec1)) 1)
    (check = (dvec-length (dvec1)) 1))

  (test-case "test_vec1_operator_increment"
    (define v0 (ivec1 1))
    (define v1 (ivec1 v0))
    (define v2 (ivec1 v0))
    (define v3 (++ivec! v1))
    (define v4 (ivec++! v2))
    (check equal? v0 v4)
    (check equal? v1 v2)
    (check equal? v1 v3))

  ;; ---------------------------------------------------------------------------

  (define-syntax-rule (check-vec2-operator-AC [a vecop c vecop=! want] ...)
    (begin
      (let* ([A (vec2 a)]
             [C (vecop A c)])
        (vecop=! A c)
        (check equal? A (vec2 want))
        (check equal? A C)) ...))

  (define-syntax-rule (check-vec2-operator-ABC [a vecop (b ...) vecop=! (want ...)] ...)
    (begin
      (let* ([A (vec2 a)]
             [B (vec2 b ...)]
             [C (vecop A B)])
        (vecop=! A B)
        (check equal? A (vec2 want ...))
        (check equal? A C)) ...))

  (define-syntax-rule (check-operator-AB
                       [*vec (a ...) vecop=! (b ...) (w ...) x (v ...)] ...)
    (begin
      (let ([A (*vec a ...)]
            [B (*vec b ...)])
        (vecop=! A B)
        (check equal? A (*vec w ...))
        (vecop=! A x)
        (check equal? A (*vec v ...))) ...))

  (define-syntax-rule (check-unop [unop! (a ...) (a* ...) (b ...)] ...)
    (begin
      (let* ([A (ivec2 1 2)]
             [B (unop! A)])
        (check equal? A (ivec2 a* ...))
        (check equal? B (ivec2 b ...))) ...))

  (test-case "vec2 test_operators"
    (check equal? (ivec2 1) (ivec2 1))
    (check-vec2-operator-AC
     [1 vec+ 1 vec+=! 2]
     [1 vec- 1 vec-=! 0]
     [1 vec* 2 vec*=! 2])
    (check-vec2-operator-ABC
     [1 vec+ (2 -1) vec+=! ( 3 0)]
     [1 vec- (2 -1) vec-=! (-1 2)]
     [2 vec/ (2)    vec/=! ( 1)  ])
    (let ([A (vec2 1 2)]
          [B (vec2 4 5)])
      (check equal? (vec+ A B) (vec2 5 7))
      (check equal? (vec- B A) (vec2 3 3))
      (check equal? (vec* A B) (vec2 4 10))
      (check equal? (vec/ B A) (vec2 4 2.5))
      (check equal? (vec+ A 1) (vec2 2 3))
      (check equal? (vec- B 1) (vec2 3 4))
      (check equal? (vec* A 2) (vec2 2 4))
      (check equal? (vec/ B 2) (vec2 2 2.5))
      (check equal? (vec+ 1 A) (vec2 2 3))
      (check equal? (vec- 1 B) (vec2 -3 -4))
      (check equal? (vec* 2 A) (vec2 2 4))
      (check equal? (vec/ 2 B) (vec2 0.5 2/5)))
    (check-operator-AB
     [ vec2 (1 2)  vec+=! (4 5) (5 7) 1 (6 8)]
     [ivec2 (4 5) ivec-=! (1 2) (3 3) 1 (2 2)]
     [ivec2 (1 2) ivec*=! (4 5) (4 10) 2 (8 20)]
     [ivec2 (4 16) ivec/=! (1 2) (4 8) 2 (2 4)])
    (let ([B (ivec2 2)])
      (ivec/=! B (ivec-ref B 1))
      (check equal? B (ivec2 1)))
    (check equal? (ivec- (ivec2 1 2)) (ivec2 -1 -2))
    (check-unop
     [--ivec! (1 2) (0 1) (0 1)]
     [ivec--! (1 2) (0 1) (1 2)]
     [++ivec! (1 2) (2 3) (2 3)]
     [ivec++! (1 2) (2 3) (1 2)]))

  (test-case "vec2 test_ctor"
    (let ([A (ivec2 1)]) (check equal? A (ivec2 A)))
    (let* ([R (vec1 1)]
           [S (vec1 2)]
           [O (vec2 1 2)]
           [A (vec2 R)])
      (check equal? A (vec2 1))
      (check equal? (vec2 R S) O)
      (check equal? (vec2 R 2) O)
      (check equal? (vec2 1 S) O)))

  (test-case "vec2 test_size"
    (check = (vec-length (vec2)) 2)
    (check = (dvec-length (dvec2)) 2))

  (test-case "vec2 test_operator_increment"
    (define v0 (ivec2 1))
    (define v1 (ivec2 v0))
    (define v2 (ivec2 v0))
    (define v3 (++ivec! v1))
    (define v4 (ivec++! v2))
    (check equal? v0 v4)
    (check equal? v1 v2)
    (check equal? v1 v3))

  ;; ---------------------------------------------------------------------------

  (test-case "test_vec3_ctor"
    (check equal? (ivec3 1) (ivec3 1 1 1))
    (for ([v (in-list (list (ivec3 (ivec2 1 2) 3)
                            (ivec3 1 (ivec2 2 3))
                            (ivec3 1 2 3)
                            (ivec3 (ivec4 1 2 3 4))))])
      (check equal? v (ivec3 1 2 3)))
    (let* ([R (vec1 1)]
           [S (vec1 2)]
           [T (vec1 3)]
           [O (vec3 1 2 3)]
           [A (vec3 R)])
      (check equal? A (vec3 1))
      (check equal? (vec3 R S T) O)
      (check equal? (vec3 R 2 3) O)
      (check equal? (vec3 1 S 3) O)
      (check equal? (vec3 1 2 T) O)
      (check equal? (vec3 1 S T) O)
      (check equal? (vec3 R 2 T) O)
      (check equal? (vec3 R S 3) O)))

  (test-case "test_bvec3_ctor"
    (define A (bvec3 #t))
    (define B (bvec3 #t))
    (define C (bvec3 #f))
    (check equal? (bvec-and A B) (bvec3 #t))
    (check equal? (bvec-and A C) (bvec3 #f))
    (check equal? (bvec-or A C) (bvec3 #t))
    (check equal? (bvec-not A) C))

  (test-case "test_vec3_operators"
    (check equal? (ivec3 1) (ivec3 1))
    (let ([A (vec3 1 2 3)]
          [B (vec3 4 5 6)])
      (check equal? (vec+ A B) (vec3 5 7 9))
      (check equal? (vec- B A) (vec3 3 3 3))
      (check equal? (vec* A B) (vec3 4 10 18))
      (check equal? (vec/ B A) (vec3 4 2.5 2))
      (check equal? (vec+ A 1) (vec3 2 3 4))
      (check equal? (vec- B 1) (vec3 3 4 5))
      (check equal? (vec* A 2) (vec3 2 4 6))
      (check equal? (vec/ B 2) (vec3 2 2.5 3))
      (check equal? (vec+ 1 A) (vec3 2 3 4))
      (check equal? (vec- 1 B) (vec3 -3 -4 -5))
      (check equal? (vec* 2 A) (vec3 2 4 6))
      (check equal? (vec/ 2 B) (vec3 0.5 2/5 2/6)))
    (let ([A (ivec3 1.0 2.0 3.0)]
          [B (ivec3 4.0 5.0 6.0)])
      (ivec+=! A B) (check equal? A (ivec3 5 7 9))
      (ivec+=! A 1) (check equal? A (ivec3 6 8 10)))
    (let ([A (ivec3 1.0 2.0 3.0)]
          [B (ivec3 4.0 5.0 6.0)])
      (ivec-=! B A) (check equal? B (ivec3 3 3 3))
      (ivec-=! B 1) (check equal? B (ivec3 2 2 2)))
    (let ([A (ivec3 1.0 2.0 3.0)]
          [B (ivec3 4.0 5.0 6.0)])
      (ivec*=! A B) (check equal? A (ivec3 4 10 18))
      (ivec*=! A 2) (check equal? A (ivec3 8 20 36)))
    (let ([A (ivec3 1.0 2.0 3.0)]
          [B (ivec3 4.0 4.0 6.0)])
      (ivec/=! B A) (check equal? B (ivec3 4 2 2))
      (ivec/=! B 2) (check equal? B (ivec3 2 1 1)))
    (let ([B (ivec3 2)])
      (ivec/=! B (ivec-ref B 1))
      (check equal? B (ivec3 1)))
    (let* ([A (ivec3 1.0 2.0 3.0)]
           [B (ivec- A)])
      (check equal? (ivec- A) (ivec3 -1.0 -2.0 -3.0)))
    (let* ([A (ivec3 1.0 2.0 3.0)]
           [B (--ivec! A)])
      (check equal? B (ivec3 0.0 1.0 2.0)))
    (let* ([A (ivec3 1.0 2.0 3.0)]
           [B (ivec--! A)])
      (check equal? B (ivec3 1.0 2.0 3.0))
      (check equal? A (ivec3 0.0 1.0 2.0)))
    (let* ([A (ivec3 1.0 2.0 3.0)]
           [B (++ivec! A)])
      (check equal? B (ivec3 2.0 3.0 4.0)))
    (let* ([A (ivec3 1.0 2.0 3.0)]
           [B (ivec++! A)])
      (check equal? B (ivec3 1.0 2.0 3.0))
      (check equal? A (ivec3 2.0 3.0 4.0))))

  (test-case "test_vec3_size"
    (check = (vec-length (vec3)) 3)
    (check = (dvec-length (dvec3)) 3))

  (test-case "vec3 test_operator_increment"
    (define v0 (ivec3 1))
    (define v1 (ivec3 v0))
    (define v2 (ivec3 v0))
    (define v3 (++ivec! v1))
    (define v4 (ivec++! v2))
    (check equal? v0 v4)
    (check equal? v1 v2)
    (check equal? v1 v3))

  ;; ---------------------------------------------------------------------------

  (test-case "test_vec4_ctor"
    (let* ([A (ivec4 1 2 3 4)]
           [B (ivec4 A)])
      (check equal? A B))
    (let ([A (ivec4 1)]
          [B (ivec4 1 1 1 1)])
      (check equal? A B))
    (for ([v (in-list (list (ivec4 (ivec2 1 2) 3 4)
                            (ivec4 1 (ivec2 2 3) 4)
                            (ivec4 1 2 (ivec2 3 4))
                            (ivec4 (ivec3 1 2 3) 4)
                            (ivec4 1 (ivec3 2 3 4))
                            (ivec4 (ivec2 1 2) (ivec2 3 4))
                            (ivec4 1 2 3 4)
                            (ivec4 (ivec4 1 2 3 4))))])
      (check equal? v (ivec4 1 2 3 4)))
    (let ([R (vec1 1.0)]
          [S (vec1 2.0)]
          [T (vec1 3.0)]
          [U (vec1 4.0)]
          [O (vec4 1.0 2.0 3.0 4.0)])
      (check equal? (vec4 R) (vec4 1.0))
      (check equal? (vec4 R S T U) O)
      (check equal? (vec4 R 2.0 3.0 4.0) O)
      (check equal? (vec4 1.0 S 3.0 4.0) O)
      (check equal? (vec4 R S 3.0 4.0) O)
      (check equal? (vec4 1.0 2.0 T 4.0) O)
      (check equal? (vec4 R 2.0 T 4.0) O)
      (check equal? (vec4 1.0 S T 4.0) O)
      (check equal? (vec4 R S T 4.0) O)
      (check equal? (vec4 R 2.0 3.0 U) O)
      (check equal? (vec4 1.0 S 3.0 U) O)
      (check equal? (vec4 R S 3.0 U) O)
      (check equal? (vec4 1.0 2.0 T U) O)
      (check equal? (vec4 R 2.0 T U) O)
      (check equal? (vec4 1.0 S T U) O)
      (check equal? (vec4 R S T U) O))
    (let ([v1_0 (vec1 1.0)]
          [v1_1 (vec1 2.0)]
          [v1_2 (vec1 3.0)]
          [v1_3 (vec1 4.0)]
          [v2_0 (vec2 1.0 2.0)]
          [v2_1 (vec2 2.0 3.0)]
          [v2_2 (vec2 3.0 4.0)]
          [v3_0 (vec3 1.0 2.0 3.0)]
          [v3_1 (vec3 2.0 3.0 4.0)]
          [O (vec4 1.0 2.0 3.0 4.0)])
      (check equal? (vec4 v1_0 v1_1 v2_2) O)
      (check equal? (vec4 1.0 2.0 v2_2) O)
      (check equal? (vec4 v1_0 2.0 v2_2) O)
      (check equal? (vec4 1.0 v1_1 v2_2) O)
      (check equal? (vec4 v2_0 v1_2 v1_3) O)
      (check equal? (vec4 v2_0 3.0 v1_3) O)
      (check equal? (vec4 v2_0 v1_2 4.0) O)
      (check equal? (vec4 v2_0 3.0 4.0) O))
    (let ([v1_0 (vec1 1.0)]
          [v1_1 (vec1 2.0)]
          [v1_2 (vec1 3.0)]
          [v1_3 (vec1 4.0)]
          [v2 (vec2 2.0 3.0)]
          [O (vec4 1.0 2.0 3.0 4.0)])
      (check equal? (vec4 v1_0 v2 v1_3) O)
      (check equal? (vec4 v1_0 v2 4.0) O)
      (check equal? (vec4 1.0 v2 v1_3) O)
      (check equal? (vec4 1.0 v2 4.0) O))
    )

  (test-case "test_bvec4_ctor"
    (let ([A (bvec4 #t)]
          [B (bvec4 #t)]
          [C (bvec4 #f)])
      (check equal? (bvec-and A B) (bvec4 #t))
      (check equal? (bvec-and A C) (bvec4 #f))
      (check equal? (bvec-or A C) (bvec4 #t))
      (check-false (equal? A C))))

  (test-case "vec4 test_operators"
    (check equal? (ivec4 1) (ivec4 1))
    (let ([A (vec4 1.0 2.0 3.0 4.0)]
          [B (vec4 4.0 5.0 6.0 7.0)])
      (check equal? (vec+ A B) (vec4 5 7 9 11))
      (check equal? (vec- B A) (vec4 3 3 3 3))
      (check equal? (vec* A B) (vec4 4 10 18 28))
      (check equal? (vec/ B A) (vec4 4 2.5 2 7/4))
      (check equal? (vec+ A 1.0) (vec4 2 3 4 5))
      (check equal? (vec- B 1.0) (vec4 3 4 5 6))
      (check equal? (vec* A 2.0) (vec4 2 4 6 8))
      (check equal? (vec/ B 2.0) (vec4 2 2.5 3 3.5))
      (check equal? (vec+ 1.0 A) (vec4 2 3 4 5))
      (check equal? (vec- 1.0 B) (vec4 -3 -4 -5 -6))
      (check equal? (vec* 2.0 A) (vec4 2 4 6 8))
      (check equal? (vec/ 2.0 B) (vec4 0.5 2/5 2/6 2/7)))
    (let ([A (ivec4 1.0 2.0 3.0 4.0)]
          [B (ivec4 4.0 5.0 6.0 7.0)])
      (ivec+=! A B) (check equal? A (ivec4 5 7 9 11))
      (ivec+=! A 1) (check equal? A (ivec4 6 8 10 12)))
    (let ([A (ivec4 1.0 2.0 3.0 4.0)]
          [B (ivec4 4.0 5.0 6.0 7.0)])
      (ivec-=! B A) (check equal? B (ivec4 3 3 3 3))
      (ivec-=! B 1) (check equal? B (ivec4 2 2 2 2)))
    (let ([A (ivec4 1.0 2.0 3.0 4.0)]
          [B (ivec4 4.0 5.0 6.0 7.0)])
      (ivec*=! A B) (check equal? A (ivec4 4 10 18 28))
      (ivec*=! A 2) (check equal? A (ivec4 8 20 36 56)))
    (let ([A (ivec4 1.0 2.0 2.0 4.0)]
          [B (ivec4 4.0 4.0 8.0 8.0)])
      (ivec/=! B A) (check equal? B (ivec4 4 2 4 2))
      (ivec/=! B 2) (check equal? B (ivec4 2 1 2 1)))
    (let ([B (ivec4 2)])
      (ivec/=! B (ivec-ref B 1))
      (check equal? B (ivec4 1)))
    (let* ([A (ivec4 1.0 2.0 3.0 4.0)]
           [B (ivec- A)])
      (check equal? B (ivec4 -1.0 -2.0 -3.0 -4.0)))
    (let* ([A (ivec4 1.0 2.0 3.0 4.0)]
           [B (--ivec! A)])
      (check equal? B (ivec4 0.0 1.0 2.0 3.0)))
    (let* ([A (ivec4 1.0 2.0 3.0 4.0)]
           [B (ivec--! A)])
      (check equal? B (ivec4 1.0 2.0 3.0 4.0))
      (check equal? A (ivec4 0.0 1.0 2.0 3.0)))
    (let* ([A (ivec4 1.0 2.0 3.0 4.0)]
           [B (++ivec! A)])
      (check equal? B (ivec4 2.0 3.0 4.0 5.0)))
    (let* ([A (ivec4 1.0 2.0 3.0 4.0)]
           [B (ivec++! A)])
      (check equal? B (ivec4 1.0 2.0 3.0 4.0))
      (check equal? A (ivec4 2.0 3.0 4.0 5.0))))

  (test-case "vec4 test_equal"
    (check equal? (uvec4 1 2 3 4) (uvec4 1 2 3 4))
    (check equal? (ivec4 1 2 3 4) (ivec4 1 2 3 4)))

  (test-case "vec4 test_size"
    (check = (vec-length (vec4)) 4)
    (check = (dvec-length (dvec4)) 4))

  (test-case "vec4 test_operator_increment"
    (define v0 (ivec4 1))
    (define v1 (ivec4 v0))
    (define v2 (ivec4 v0))
    (define v3 (++ivec! v1))
    (define v4 (ivec++! v2))
    (check equal? v0 v4)
    (check equal? v1 v2)
    (check equal? v1 v3)))
