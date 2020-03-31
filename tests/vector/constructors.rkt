#lang racket/base

(require glm/vector)

(module+ test
  (require glm/private/testing glm/scalar template (for-syntax racket/base))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (for*/template ([$ '(b d i u)]
                  [N '(1 2 3 4)])

    (define-syntax-rule (check-$vecN/0)
      (let ([v ($vecN)])
        (check-pred $vecN? v)
        (check equal? ($vecN->list v) (build-list N (λ _ ($scalar 0))))))

    (define-syntax-rule (check-$vecN/1 (i) expr)
      (for ([i (in-range -2 3)])
        (define v expr)
        (check-pred $vecN? v)
        (check equal? ($vecN->list v) (build-list N (λ _ ($scalar i))))))

    (for*/template ([I '(1 2 3)]
                    [J '(1 2 3)])
      (when-template (= (+ I J) N)
        (define-syntax-rule (check-$vecN/I+J (i j) expr)
          (for* ([i (in-range -2 3)]
                 [j (in-range -2 3)])
            (define v expr)
            (check-pred $vecN? v)
            (check equal? ($vecN->list v)
                   (append (build-list I (λ _ ($scalar i)))
                           (build-list J (λ _ ($scalar j)))))))))

    (for*/template ([I '(1 2)]
                    [J '(1 2)]
                    [K '(1 2)])
      (when-template (= (+ I J K) N)
        (define-syntax-rule (check-$vecN/I+J+K (i j k) expr)
          (for* ([i (in-range -2 3)]
                 [j (in-range -2 3)]
                 [k (in-range -2 3)])
            (define v expr)
            (check-pred $vecN? v)
            (check equal? ($vecN->list v)
                   (append (build-list I (λ _ ($scalar i)))
                           (build-list J (λ _ ($scalar j)))
                           (build-list K (λ _ ($scalar k)))))))))

    (define-syntax-rule (check-$vecN/1+1+1+1 (i j k l) expr)
      (for* ([i (in-range -2 3)]
             [j (in-range -2 3)]
             [k (in-range -2 3)]
             [l (in-range -2 3)])
        (define v expr)
        (check-pred $vecN? v)
        (check equal? ($vecN->list v)
               (list ($scalar i) ($scalar j) ($scalar k) ($scalar l)))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define-syntax-rule (check-$vecN/scalar)
      (check-$vecN/1 (i) ($vecN i)))

    (for/template ([M '(1 2 3 4)])
      (define-syntax-rule (check-$vecN/vecM)
        (check-$vecN/1 (i) ($vecN ($vecM i)))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define-syntax-rule (check-$vecN/scalar+scalar)
      (check-$vecN/1+1 (i j) ($vecN i j)))

    (for/template ([M '(1 2 3)])
      (define-syntax-rule (check-$vecN/scalar+vecM)
        (check-$vecN/1+M (i j) ($vecN i ($vecM j))))
      (define-syntax-rule (check-$vecN/vecM+scalar)
        (check-$vecN/M+1 (i j) ($vecN ($vecM i) j)))
      (for/template ([P '(1 2 3)])
        (define-syntax-rule (check-$vecN/vecM+vecP)
          (check-$vecN/M+P (i j) ($vecN ($vecM i) ($vecP j))))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define-syntax-rule (check-$vecN/scalar+scalar+scalar)
      (check-$vecN/1+1+1 (i j k) ($vecN i j k)))

    (for/template ([M '(1 2)])
      (when-template (= (+ M 2) N)
        (define-syntax-rule (check-$vecN/scalar+scalar+vecM)
          (check-$vecN/1+1+M (i j k) ($vecN i j ($vecM k))))
        (define-syntax-rule (check-$vecN/scalar+vecM+scalar)
          (check-$vecN/1+M+1 (i j k) ($vecN i ($vecM j) k)))
        (define-syntax-rule (check-$vecN/vecM+scalar+scalar)
          (check-$vecN/M+1+1 (i j k) ($vecN ($vecM i) j k))))
      (for/template ([P '(1 2)])
        (when-template (= (+ M P 1) N)
          (define-syntax-rule (check-$vecN/vecM+vecP+scalar)
            (check-$vecN/M+P+1 (i j k) ($vecN ($vecM i) ($vecP j) k)))
          (define-syntax-rule (check-$vecN/vecM+scalar+vecP)
            (check-$vecN/M+1+P (i j k) ($vecN ($vecM i) j ($vecP k))))
          (define-syntax-rule (check-$vecN/scalar+vecM+vecP)
            (check-$vecN/1+M+P (i j k) ($vecN i ($vecM j) ($vecP k)))))
        (for/template ([Q '(1 2)])
          (when-template (= (+ M P Q) N)
            (define-syntax-rule (check-$vecN/vecM+vecP+vecQ)
              (check-$vecN/M+P+Q (i j k) ($vecN ($vecM i) ($vecP j) ($vecQ k))))))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define-syntax-rule (check-$vecN/scalar+scalar+scalar+scalar)
      (check-$vecN/1+1+1+1 (i j k l) ($vecN i j k l)))

    (define-syntax-rule (check-$vecN/scalar+scalar+scalar+vec1)
      (check-$vecN/1+1+1+1 (i j k l) ($vecN i j k ($vec1 l))))
    (define-syntax-rule (check-$vecN/scalar+scalar+vec1+scalar)
      (check-$vecN/1+1+1+1 (i j k l) ($vecN i j ($vec1 k) l)))
    (define-syntax-rule (check-$vecN/scalar+vec1+scalar+scalar)
      (check-$vecN/1+1+1+1 (i j k l) ($vecN i ($vec1 j) k l)))
    (define-syntax-rule (check-$vecN/vec1+scalar+scalar+scalar)
      (check-$vecN/1+1+1+1 (i j k l) ($vecN ($vec1 i) j k l)))

    (define-syntax-rule (check-$vecN/scalar+scalar+vec1+vec1)
      (check-$vecN/1+1+1+1 (i j k l) ($vecN i j ($vec1 k) ($vec1 l))))
    (define-syntax-rule (check-$vecN/scalar+vec1+scalar+vec1)
      (check-$vecN/1+1+1+1 (i j k l) ($vecN i ($vec1 j) k ($vec1 l))))
    (define-syntax-rule (check-$vecN/vec1+scalar+scalar+vec1)
      (check-$vecN/1+1+1+1 (i j k l) ($vecN ($vec1 i) j k ($vec1 l))))
    (define-syntax-rule (check-$vecN/scalar+vec1+vec1+scalar)
      (check-$vecN/1+1+1+1 (i j k l) ($vecN i ($vec1 j) ($vec1 k) l)))
    (define-syntax-rule (check-$vecN/vec1+scalar+vec1+scalar)
      (check-$vecN/1+1+1+1 (i j k l) ($vecN ($vec1 i) j ($vec1 k) l)))
    (define-syntax-rule (check-$vecN/vec1+vec1+scalar+scalar)
      (check-$vecN/1+1+1+1 (i j k l) ($vecN ($vec1 i) ($vec1 j) k l)))

    (define-syntax-rule (check-$vecN/scalar+vec1+vec1+vec1)
      (check-$vecN/1+1+1+1 (i j k l) ($vecN i ($vec1 j) ($vec1 k) ($vec1 l))))
    (define-syntax-rule (check-$vecN/vec1+scalar+vec1+vec1)
      (check-$vecN/1+1+1+1 (i j k l) ($vecN ($vec1 i) j ($vec1 k) ($vec1 l))))
    (define-syntax-rule (check-$vecN/vec1+vec1+scalar+vec1)
      (check-$vecN/1+1+1+1 (i j k l) ($vecN ($vec1 i) ($vec1 j) k ($vec1 l))))
    (define-syntax-rule (check-$vecN/vec1+vec1+vec1+scalar)
      (check-$vecN/1+1+1+1 (i j k l) ($vecN ($vec1 i) ($vec1 j) ($vec1 k) l)))

    (define-syntax-rule (check-$vecN/vec1+vec1+vec1+vec1)
      (check-$vecN/1+1+1+1 (i j k l) ($vecN ($vec1 i) ($vec1 j) ($vec1 k) ($vec1 l)))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-tests "constructors"
    (for/template ([T '(boolean double float int uint)]
                   [$ '(b d i u)])

      (test-suite "T"
        (test-suite "$vec1"
          (test-case "no args" (check-$vec1/0))
          (test-suite "one arg"
            (test-case "scalar" (check-$vec1/scalar))
            (test-case "vec1" (check-$vec1/vec1))
            (test-case "vec2" (check-$vec1/vec2))
            (test-case "vec3" (check-$vec1/vec3))
            (test-case "vec4" (check-$vec1/vec4))))

        (test-suite "$vec2"
          (test-case "no args" (check-$vec2/0))
          (test-suite "one arg"
            (test-case "scalar" (check-$vec2/scalar))
            (test-case "vec1" (check-$vec2/vec1))
            (test-case "vec2" (check-$vec2/vec2))
            (test-case "vec3" (check-$vec2/vec3))
            (test-case "vec4" (check-$vec2/vec4)))

          (test-suite "two args"
            (for*/template ([A '(scalar vec1)]
                            [B '(scalar vec1)])
              (test-case "A+B" (check-$vec2/A+B)))))

        (test-suite "$vec3"
          (test-case "no args" (check-$vec3/0))
          (test-suite "one arg"
            (test-case "scalar" (check-$vec3/scalar))
            (test-case "vec1" (check-$vec3/vec1))
            (test-case "vec3" (check-$vec3/vec3))
            (test-case "vec4" (check-$vec3/vec4)))
          (test-suite "two args"
            (for/template ([A '(scalar vec1 vec2)]
                           [I '(1 1 2)])
              (for/template ([B '(scalar vec1 vec2)]
                             [J '(1 1 2)])
                (when-template (= (+ I J) 3)
                  (test-case "A+B" (check-$vec3/A+B))))))
          (test-suite "three args"
            (for/template ([A '(scalar vec1 vec2)]
                           [I '(1 1 2)])
              (for/template ([B '(scalar vec1 vec2)]
                             [J '(1 1 2)])
                (for/template ([C '(scalar vec1 vec2)]
                               [K '(1 1 2)])
                  (when-template (= (+ I J K) 3)
                    (test-case "A+B+C" (check-$vec3/A+B+C))))))))

        (test-suite "$vec4"
          (test-case "no args" (check-$vec4/0))
          (test-suite "one arg"
            (test-case "scalar" (check-$vec4/scalar))
            (test-case "vec1" (check-$vec4/vec1))
            (test-case "vec4" (check-$vec4/vec4)))

          (test-suite "two args"
            (for/template ([A '(scalar vec1 vec2 vec3)]
                           [I '(1 1 2 3)])
              (for/template ([B '(scalar vec1 vec2 vec3)]
                             [J '(1 1 2 3)])
                (when-template (= (+ I J) 4)
                  (test-case "A+B" (check-$vec4/A+B))))))

          (test-suite "three args"
            (for/template ([A '(scalar vec1 vec2)]
                           [I '(1 1 2)])
              (for/template ([B '(scalar vec1 vec2)]
                             [J '(1 1 2)])
                (for/template ([C '(scalar vec1 vec2)]
                               [K '(1 1 2)])
                  (when-template (= (+ I J K) 4)
                    (test-case "A+B+C" (check-$vec4/A+B+C)))))))

          (test-suite "four args"
            (for*/template ([A '(scalar vec1)]
                            [B '(scalar vec1)]
                            [C '(scalar vec1)]
                            [D '(scalar vec1)])
              (test-case "A+B+C+D" (check-$vec4/A+B+C+D))))))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (run-all-tests))
