#lang racket/base

(require glm)

(module+ test
  (require rackunit
           rackunit/text-ui
           template
           (for-syntax racket/base))

  (define (run-all-tests)
    (run-tests vector-tests)
    (void))

  (begin-template
    (define-test-suite vector-tests
      (for*/template ([$ (in-list '(b d || i u))]
                      [N (in-list '(1 2 3 4))])
        $vecN-tests)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (for/template ([$ (in-list '(b d || i u))])
    (for/template ([N (in-list '(1 2 3 4))])

      (define-syntax-rule (check-$vecN/1 (i) expr)
        (for ([i (in-range -2 3)])
          (define v expr)
          (check-pred $vecN? v)
          (check equal? ($vecN->list v) (build-list N (λ _ ($scalar i))))))

      (define-syntax-rule (check-$vecN/2 (i j) expr)
        (for ([i (in-range -2 3)]
              [j (in-range -2 3)])
          (define v expr)
          (check-pred $vecN? v)
          (check equal? ($vecN->list v) (map $scalar (list i j)))))

      (define-syntax-rule (check-$vecN/3 (i j k) expr)
        (for ([i (in-range -2 3)]
              [j (in-range -2 3)]
              [k (in-range -2 3)])
          (define v expr)
          (check-pred $vecN? v)
          (check equal? ($vecN->list v) (map $scalar (list i j k)))))

      (define-syntax-rule (check-$vecN/4 (i j k l) expr)
        (for ([i (in-range -2 3)]
              [j (in-range -2 3)]
              [k (in-range -2 3)]
              [l (in-range -2 3)])
          (define v expr)
          (check-pred $vecN? v)
          (check equal? ($vecN->list v) (map $scalar (list i j k l)))))

      ;; -----------------------------------------------------------------------

      (define-syntax-rule (check-$vecN/scalar)
        (check-$vecN/1 (i) ($vecN i)))

      (for/template ([M (in-list '(1 2 3 4))])
        (define-syntax-rule (check-$vecN/vecM)
          (check-$vecN/1 (i) ($vecN ($vecM i)))))

      ;; -----------------------------------------------------------------------

      (for*/template ([I (in-list '(1 2 3 4))]
                      [J (in-list '(1 2 3 4))])
        (define-syntax-rule (check-$vecN/I+J (i j) expr)
          (for* ([i (in-range -2 3)]
                 [j (in-range -2 3)])
            (define v expr)
            (check-pred $vecN? v)
            (check equal? ($vecN->list v) (append (build-list I (λ _ ($scalar i)))
                                                  (build-list J (λ _ ($scalar j))))))))

      (define-syntax-rule (check-$vecN/scalar+scalar)
        (check-$vecN/1+1 (i j) ($vecN i j)))

      (for/template ([M (in-list '(1 2 3 4))])
        (define-syntax-rule (check-$vecN/scalar+vecM)
          (check-$vecN/1+M (i j) ($vecN i ($vecM j))))

        (define-syntax-rule (check-$vecN/vecM+scalar)
          (check-$vecN/M+1 (i j) ($vecN ($vecM i) j)))

        (for/template ([P (in-list '(1 2 3 4))])
          (define-syntax-rule (check-$vecN/vecM+vecP)
            (check-$vecN/M+P (i j) ($vecN ($vecM i) ($vecP j))))))

      ;; -----------------------------------------------------------------------

      (for*/template ([I (in-list '(1 2 3 4))]
                      [J (in-list '(1 2 3 4))]
                      [K (in-list '(1 2 3 4))])
        (define-syntax-rule (check-$vecN/I+J+K (i j k) expr)
          (for* ([i (in-range -2 3)]
                 [j (in-range -2 3)]
                 [k (in-range -2 3)])
            (define v expr)
            (check-pred $vecN? v)
            (check equal? ($vecN->list v) (append (build-list I (λ _ ($scalar i)))
                                                  (build-list J (λ _ ($scalar j)))
                                                  (build-list K (λ _ ($scalar k))))))))

      (define-syntax-rule (check-$vecN/scalar+scalar+scalar)
        (check-$vecN/1+1+1 (i j k) ($vecN i j k)))

      (for/template ([M (in-list '(1 2))])
        (when-template (= (+ M 2) N)
          (define-syntax-rule (check-$vecN/vecM+scalar+scalar)
            (check-$vecN/M+1+1 (i j k) ($vecN ($vecM i) j k)))

          (define-syntax-rule (check-$vecN/scalar+vecM+scalar)
            (check-$vecN/1+M+1 (i j k) ($vecN i ($vecM j) k)))

          (define-syntax-rule (check-$vecN/scalar+scalar+vecM)
            (check-$vecN/1+1+M (i j k) ($vecN i j ($vecM k))))))

      (for*/template ([M (in-list '(1 2))]
                      [P (in-list '(1 2))])
        (when-template (= (+ M P 1) N)
          (define-syntax-rule (check-$vecN/scalar+vecM+vecP)
            (check-$vecN/1+M+P (i j k) ($vecN i ($vecM j) ($vecP k))))

          (define-syntax-rule (check-$vecN/vecM+scalar+vecP)
            (check-$vecN/M+1+P (i j k) ($vecN ($vecM i) j ($vecP k))))

          (define-syntax-rule (check-$vecN/vecM+vecP+scalar)
            (check-$vecN/M+P+1 (i j k) ($vecN ($vecM i) ($vecP j) k)))))

      (for*/template ([M (in-list '(1 2))]
                      [P (in-list '(1 2))]
                      [Q (in-list '(1 2))])
        (when-template (= (+ M P Q) N)
          (define-syntax-rule (check-$vecN/vecM+vecP+vecQ)
            (check-$vecN/M+P+Q (i j k) ($vecN ($vecM i) ($vecP j) ($vecQ k))))))

      ;; -----------------------------------------------------------------------

      (for*/template ([I (in-list '(1 2 3 4))]
                      [J (in-list '(1 2 3 4))]
                      [K (in-list '(1 2 3 4))]
                      [L (in-list '(1 2 3 4))])
        (define-syntax-rule (check-$vecN/I+J+K+L (i j k l) expr)
          (for* ([i (in-range -2 3)]
                 [j (in-range -2 3)]
                 [k (in-range -2 3)]
                 [l (in-range -2 3)])
            (define v expr)
            (check-pred $vecN? v)
            (check equal? ($vecN->list v) (append (build-list I (λ _ ($scalar i)))
                                                  (build-list J (λ _ ($scalar j)))
                                                  (build-list K (λ _ ($scalar k)))
                                                  (build-list L (λ _ ($scalar l))))))))

      (when-template (= N 4)
        (for*/template ([A (in-list '(scalar vec1))]
                        [B (in-list '(scalar vec1))]
                        [C (in-list '(scalar vec1))]
                        [D (in-list '(scalar vec1))])
          (define-syntax-rule (check-$vec4/A+B+C+D)
            (check-$vecN/1+1+1+1 (i j k l) ($vec4 i j k l)))))

      ;; -----------------------------------------------------------------------

      (define-test-suite $vecN-tests
        (test-case "$vecN?"
          (check-false ($vecN? -1))
          (for/template ([M (in-list '(1 2 3 4))])
            ((if-template (= M N) check-true check-false) ($vecN? ($vecM)))))

        (test-suite "constructors"
          (test-case "0 args"
            (define v ($vecN))
            (check-pred $vecN? v)
            (check equal? ($vecN->list v) (build-list N (λ _ ($scalar 0)))))

          (when-template (= N 1)
            (test-suite "one arg"
              (test-case "scalar" (check-$vec1/scalar))
              (test-case "vec1" (check-$vec1/vec1))))

          (when-template (= N 2)
            (test-suite "one arg"
              (test-case "scalar" (check-$vec2/scalar))
              (test-case "vec1" (check-$vec2/vec1))
              (test-case "vec2" (check-$vec2/vec2)))

              (test-suite "two args"
                (for*/template ([A (in-list '(scalar vec1))]
                                [B (in-list '(scalar vec1))])
                  (test-case "A+B" (check-$vec2/A+B)))))

          (when-template (= N 3)
            (test-suite "one arg"
              (test-case "scalar" (check-$vec3/scalar))
              (test-case "vec1" (check-$vec3/vec1))
              (test-case "vec3" (check-$vec3/vec3)))

            (test-suite "two args"
              (for/template ([A (in-list '(scalar vec1 vec2 vec2))]
                             [B (in-list '(vec2 vec2 vec1 scalar))])
                (test-case "A+B" (check-$vec3/A+B))))

            (test-suite "three args"
              (for*/template ([A (in-list '(scalar vec1))]
                              [B (in-list '(scalar vec1))]
                              [C (in-list '(scalar vec1))])
                (test-case "A+B+C" (check-$vec3/A+B+C)))))

          (when-template (= N 4)
            (test-suite "one arg"
              (test-case "scalar" (check-$vec4/scalar))
              (test-case "vec1" (check-$vec4/vec1))
              (test-case "vec4" (check-$vec4/vec4)))

            (test-suite "two args"
              (for/template ([A (in-list '(scalar vec1 vec2 vec3 vec3))]
                             [B (in-list '(vec3 vec3 vec2 vec1 scalar))])
                (test-case "A+B" (check-$vec4/A+B))))

            (test-suite "three args"
              (for*/template ([I (in-list '(1 2))]
                              [J (in-list '(1 2))]
                              [K (in-list '(1 2))])
                (when-template (= (+ I J K) 4)
                  (cond-template
                    [(= (+ I J) 2)
                     (test-case "scalar+scalar+vecK" (check-vec4/scalar+scalar+vecK))]
                    [(= (+ I K) 2)
                     (test-case "scalar+vecJ+scalar" (check-vec4/scalar+vecJ+scalar))]
                    [(= (+ J K) 2)
                     (test-case "vecI+scalar+scalar" (check-vec4/vecI+scalar+scalar))]
                    [(= I 1) (test-case "scalar+vecJ+vecK" (check-$vec4/scalar+vecJ+vecK))]
                    [(= J 1) (test-case "vecI+scalar+vecK" (check-$vec4/vecI+scalar+vecK))]
                    [(= K 1) (test-case "vecI+vecJ+scalar" (check-$vec4/vecI+vecJ+scalar))])
                  (test-case "vecI+vecJ+vecK" (check-$vec4/vecI+vecJ+vecK)))))

            (test-suite "four args"
              (for*/template ([A (in-list '(scalar vec1))]
                              [B (in-list '(scalar vec1))]
                              [C (in-list '(scalar vec1))]
                              [D (in-list '(scalar vec1))])
                (test-case "A+B+C+D" (check-$vec4/A+B+C+D))))

            ))

        ;; (cond-template
        ;;  [(eq? '$ b) ...]
        ;;  [else ...])

        )))




  ;; (for/template ([$ (in-list '(b d || i u))]
  ;;                [R-1 (in-list '(1 -1.0 -1.0 -1 4294967295))]
  ;;                [R0  (in-list '(0  0.0  0.0  0 0))]
  ;;                [R1  (in-list '(1  1.0  1.0  1 1))]
  ;;                [R2  (in-list '(1  2.0  2.0  2 2))]
  ;;                [Rt  (in-list '(1  1.0  1.0  1 1))]
  ;;                [Rf  (in-list '(0  0.0  0.0  0 0))])

  ;;   (define-test-suite $vec-tests
  ;;     (for/template ([N (in-list '(1 2 3 4))])

  ;;       (test-case "$vecN?"
  ;;         (check-false ($vecN? -1))
  ;;         (for/template ([M (in-list '(1 2 3 4))])
  ;;           ((if-template (= M N) check-true check-false) ($vecN? ($vecM)))))

  ;;       (test-case "($vecN)"
  ;;         (define v ($vecN))
  ;;         (for/template ([X (in-list '(x y z w))]
  ;;                        [R (in-list '(r g b a))]
  ;;                        [S (in-list '(s t p q))]
  ;;                        [_ (in-range N)])
  ;;           (check = ($vecN-X v) R0)
  ;;           (check = ($vecN-R v) R0)
  ;;           (check = ($vecN-S v) R0)))

  ;;       (test-case "($vecN -1)"
  ;;         (define v ($vecN -1))
  ;;         (for/template ([X (in-list '(x y z w))]
  ;;                        [R (in-list '(r g b a))]
  ;;                        [S (in-list '(s t p q))]
  ;;                        [_ (in-range N)])
  ;;           (check = ($vecN-X v) R-1)
  ;;           (check = ($vecN-R v) R-1)
  ;;           (check = ($vecN-S v) R-1)))

  ;;       (test-case "($vecN 0)"
  ;;         (define v ($vecN 0))
  ;;         (for/template ([X (in-list '(x y z w))]
  ;;                        [R (in-list '(r g b a))]
  ;;                        [S (in-list '(s t p q))]
  ;;                        [_ (in-range N)])
  ;;           (check = ($vecN-X v) R0)
  ;;           (check = ($vecN-R v) R0)
  ;;           (check = ($vecN-S v) R0)))

  ;;       (test-case "($vecN 1)"
  ;;         (define v ($vecN 1))
  ;;         (for/template ([X (in-list '(x y z w))]
  ;;                        [R (in-list '(r g b a))]
  ;;                        [S (in-list '(s t p q))]
  ;;                        [_ (in-range N)])
  ;;           (check = ($vecN-X v) R1)
  ;;           (check = ($vecN-R v) R1)
  ;;           (check = ($vecN-S v) R1)))

  ;;       (test-case "($vecN 2)"
  ;;         (define v ($vecN 2))
  ;;         (for/template ([X (in-list '(x y z w))]
  ;;                        [R (in-list '(r g b a))]
  ;;                        [S (in-list '(s t p q))]
  ;;                        [_ (in-range N)])
  ;;           (check = ($vecN-X v) R2)
  ;;           (check = ($vecN-R v) R2)
  ;;           (check = ($vecN-S v) R2)))

  ;;       (test-case "($vecN #t)"
  ;;         (define v ($vecN #t))
  ;;         (for/template ([X (in-list '(x y z w))]
  ;;                        [R (in-list '(r g b a))]
  ;;                        [S (in-list '(s t p q))]
  ;;                        [_ (in-range N)])
  ;;           (check = ($vecN-X v) Rt)
  ;;           (check = ($vecN-R v) Rt)
  ;;           (check = ($vecN-S v) Rt)))

  ;;       (test-case "($vecN #f)"
  ;;         (define v ($vecN #f))
  ;;         (for/template ([X (in-list '(x y z w))]
  ;;                        [R (in-list '(r g b a))]
  ;;                        [S (in-list '(s t p q))]
  ;;                        [_ (in-range N)])
  ;;           (check = ($vecN-X v) Rf)
  ;;           (check = ($vecN-R v) Rf)
  ;;           (check = ($vecN-S v) Rf)))

  ;;       (test-case "$vecN-ref"
  ;;         (define v (apply $vecN (build-list N values)))
  ;;         (for/template ([I (in-range N)])
  ;;           (check = ($vecN-ref v I) ($scalar I))))

  ;;       (test-case "$vecN=!"
  ;;         (define v1 ($vecN 0))
  ;;         (define v2 ($vecN 0))
  ;;         (define v3 ($vecN 0))
  ;;         (for/template ([V (in-list '(1 0 2))])
  ;;           (check-pred $vecN? ($vecN=! v1 ($vecN V)))
  ;;           (check-pred $vecN? ($vecN=$vecN! v2 ($vecN V)))
  ;;           (check-pred $vecN? ($vecN=tvecN! v3 ((if-template (eq? '$ 'i) dvecN ivecN) V)))
  ;;           (for/template ([X (in-list '(x y z w))]
  ;;                          [_ (in-range N)])
  ;;             (check = ($vecN-X v1) ($scalar V))
  ;;             (check = ($vecN-X v2) ($scalar V))
  ;;             (check = ($vecN-X v3)
  ;;                    ($scalar ((if-template (eq? '$ 'i) dscalar iscalar) V))))))

  ;;       (for/template ([⊙ (in-list '(+ - * /))]
  ;;                      [As (in-list '((2  4  7)
  ;;                                     (0 -2 -5)
  ;;                                     (1  2  6)
  ;;                                     (1 1/2 1/6)))]
  ;;                      [Bs (in-list '((1 1 1)
  ;;                                     (0 1 0)
  ;;                                     (1 1 1)
  ;;                                     (1 1 1)))])
  ;;         (test-case "$vecN⊙=!"
  ;;           (define v ($vecN 1))
  ;;           (for/template ([V (in-list '(1 2 3))]
  ;;                          [A (in-list 'As)]
  ;;                          [B (in-list 'Bs)])
  ;;             (check-pred $vecN? ($vecN⊙=! v ($vecN V)))
  ;;             (for/template ([X (in-list '(x y z w))]
  ;;                            [_ (in-range N)])
  ;;               (check = ($vecN-X v) ($scalar (if-template (eq? '$ 'b) B A)))))))

  ;;       (for/template ([⊙ (in-list '(+ - * /))]
  ;;                      [A (in-list '(2 0 1 1))]
  ;;                      [B (in-list '(1 0 1 1))])
  ;;         (test-case "$vecN⊙"
  ;;           (define v ($vecN⊙ ($vecN 1) ($vecN 1)))
  ;;           (for/template ([X (in-list '(x y z w))]
  ;;                          [_ (in-range N)])
  ;;             (check = ($vecN-X v) ($scalar (if-template (eq? '$ 'b) B A))))))

  ;;       (test-case "$vecN++!"
  ;;         (define v ($vecN))
  ;;         (check-pred $vecN? ($vecN++! v))
  ;;         (for/template ([X (in-list '(x y z w))]
  ;;                        [_ (in-range N)])
  ;;           (check = ($vecN-X v) ($scalar 1)))
  ;;         (check-pred $vecN? ($vecN++! v))
  ;;         (for/template ([X (in-list '(x y z w))]
  ;;                        [_ (in-range N)])
  ;;           (check = ($vecN-X v) ($scalar (if-template (eq? '$ 'b) 1 2)))))

  ;;       (test-case "$vecN!--"
  ;;         (define v ($vecN 1))
  ;;         (check-pred $vecN? ($vecN--! v))
  ;;         (for/template ([X (in-list '(x y z w))]
  ;;                        [_ (in-range N)])
  ;;           (check = ($vecN-X v) ($scalar 0)))
  ;;         (check-pred $vecN? ($vecN--! v))
  ;;         (for/template ([X (in-list '(x y z w))]
  ;;                        [_ (in-range N)])
  ;;           (check = ($vecN-X v) ($scalar (if-template (eq? '$ 'b) 1 -1)))))

  ;;       (test-case "$vecN%=!"
  ;;         (for/template ([K (in-list '(2 3 4))]
  ;;                        [A (in-list '(0 1 2))]
  ;;                        [B (in-list '(0 0 0))])
  ;;           (let ([v ($vecN 10)])
  ;;             (check-pred $vecN? ($vecN%=! v K))
  ;;             (for/template ([X (in-list '(x y z w))]
  ;;                            [_ (in-range N)])
  ;;               (check = ($vecN-X v) ($scalar (if-template (eq? '$ 'b) B A)))))))

  ;;       (test-case "$vecN&=!"
  ;;         (for/template ([K (in-list '(36 34 32))]
  ;;                        [A (in-list '( 4  2  0))]
  ;;                        [B (in-list '( 1  1  1))])
  ;;           (let ([v ($vecN 15)])
  ;;             (check-pred $vecN? ($vecN&=! v K))
  ;;             (for/template ([X (in-list '(x y z w))]
  ;;                            [_ (in-range N)])
  ;;               (check = ($vecN-X v) ($scalar (if-template (eq? '$ 'b) B A)))))))

  ;;       (test-case "$vecN//=!"
  ;;         (for/template ([K (in-list '(15 16 32))]
  ;;                        [A (in-list '(15 31 47))]
  ;;                        [B (in-list '( 1  1  1))])
  ;;           (let ([v ($vecN 15)])
  ;;             (check-pred $vecN? ($vecN//=! v K))
  ;;             (for/template ([X (in-list '(x y z w))]
  ;;                            [_ (in-range N)])
  ;;               (check = ($vecN-X v) ($scalar (if-template (eq? '$ 'b) B A)))))))

  ;;       (test-case "$vecN^=!"
  ;;         (for/template ([K (in-list '(0 1 2 3))]
  ;;                        [A (in-list '(3 2 1 0))]
  ;;                        [B (in-list '(1 0 0 0))])
  ;;           (let ([v ($vecN 3)])
  ;;             (check-pred $vecN? ($vecN^=! v K))
  ;;             (for/template ([X (in-list '(x y z w))]
  ;;                            [_ (in-range N)])
  ;;               (check = ($vecN-X v) ($scalar (if-template (eq? '$ 'b) B A)))))))

  ;;       (test-case "$vecN<<=!"
  ;;         (for/template ([K (in-list '(0 1 2 3))]
  ;;                        [A (in-list '(1 2 4 8))]
  ;;                        [B (in-list '(1 1 1 1))])
  ;;           (let ([v ($vecN 1)])
  ;;             (check-pred $vecN? ($vecN<<=! v K))
  ;;             (for/template ([X (in-list '(x y z w))]
  ;;                            [_ (in-range N)])
  ;;               (check = ($vecN-X v) ($scalar (if-template (eq? '$ 'b) B A)))))))

  ;;       (test-case "$vecN>>=!"
  ;;         (for/template ([K (in-list '(0 1 2 3 4))]
  ;;                        [A (in-list '(8 4 2 1 0))]
  ;;                        [B (in-list '(1 0 0 0 0))])
  ;;           (let ([v ($vecN 8)])
  ;;             (check-pred $vecN? ($vecN>>=! v K))
  ;;             (for/template ([X (in-list '(x y z w))]
  ;;                            [_ (in-range N)])
  ;;               (check = ($vecN-X v) ($scalar (if-template (eq? '$ 'b) B A)))))))

  ;;       (test-case "$vecN%"
  ;;         (for/template ([K (in-list '(2 3 4))]
  ;;                        [A (in-list '(0 1 2))]
  ;;                        [B (in-list '(0 0 0))])
  ;;           (let ([v ($vecN% ($vecN 10) K)])
  ;;             (check-pred $vecN? v)
  ;;             (for/template ([X (in-list '(x y z w))]
  ;;                            [_ (in-range N)])
  ;;               (check = ($vecN-X v) ($scalar (if-template (eq? '$ 'b) B A)))))))

  ;;       (test-case "$vecN&"
  ;;         (for/template ([K (in-list '(36 34 32))]
  ;;                        [A (in-list '( 4  2  0))]
  ;;                        [B (in-list '( 1  1  1))])
  ;;           (let ([v ($vecN& ($vecN 15) K)])
  ;;             (check-pred $vecN? v)
  ;;             (for/template ([X (in-list '(x y z w))]
  ;;                            [_ (in-range N)])
  ;;               (check = ($vecN-X v) ($scalar (if-template (eq? '$ 'b) B A)))))))

  ;;       (test-case "$vecN//"
  ;;         (for/template ([K (in-list '(15 16 32))]
  ;;                        [A (in-list '(15 31 47))]
  ;;                        [B (in-list '( 1  1  1))])
  ;;           (let ([v ($vecN// ($vecN 15) K)])
  ;;             (check-pred $vecN? v)
  ;;             (for/template ([X (in-list '(x y z w))]
  ;;                            [_ (in-range N)])
  ;;               (check = ($vecN-X v) ($scalar (if-template (eq? '$ 'b) B A)))))))

  ;;       (test-case "$vecN^"
  ;;         (for/template ([K (in-list '(0 1 2 3))]
  ;;                        [A (in-list '(3 2 1 0))]
  ;;                        [B (in-list '(1 0 0 0))])
  ;;           (let ([v ($vecN^ ($vecN 3) K)])
  ;;             (check-pred $vecN? v)
  ;;             (for/template ([X (in-list '(x y z w))]
  ;;                            [_ (in-range N)])
  ;;               (check = ($vecN-X v) ($scalar (if-template (eq? '$ 'b) B A)))))))

  ;;       (test-case "$vecN<<"
  ;;         (for/template ([K (in-list '(0 1 2 3))]
  ;;                        [A (in-list '(1 2 4 8))]
  ;;                        [B (in-list '(1 1 1 1))])
  ;;           (let ([v ($vecN<< ($vecN 1) K)])
  ;;             (check-pred $vecN? v)
  ;;             (for/template ([X (in-list '(x y z w))]
  ;;                            [_ (in-range N)])
  ;;               (check = ($vecN-X v) ($scalar (if-template (eq? '$ 'b) B A)))))))

  ;;       (test-case "$vecN>>"
  ;;         (for/template ([K (in-list '(0 1 2 3 4))]
  ;;                        [A (in-list '(8 4 2 1 0))]
  ;;                        [B (in-list '(1 0 0 0 0))])
  ;;           (let ([v ($vecN>> ($vecN 8) K)])
  ;;             (check-pred $vecN? v)
  ;;             (for/template ([X (in-list '(x y z w))]
  ;;                            [_ (in-range N)])
  ;;               (check = ($vecN-X v) ($scalar (if-template (eq? '$ 'b) B A)))))))

  ;;       (test-case "$vecN~"
  ;;         (for/template ([K (in-list '(         0          1          2))]
  ;;                        [A (in-list '(        -1         -2         -3))]
  ;;                        [U (in-list '(#xFFFFFFFF #xFFFFFFFE #xFFFFFFFD))]
  ;;                        [B (in-list '(         1          1          1))])
  ;;           (let ([v ($vecN~ ($vecN K))])
  ;;             (check-pred $vecN? v)
  ;;             (for/template ([X (in-list '(x y z w))]
  ;;                            [_ (in-range N)])
  ;;               (check = ($vecN-X v) ($scalar (if-template (eq? '$ 'b) B A)))))))

  ;;       (test-case "$vecN=?"
  ;;         (for/template ([K (in-list '( 4  5  6))]
  ;;                        [A (in-list '(#f #t #f))]
  ;;                        [B (in-list '(#t #t #t))])
  ;;           (check eq? ($vecN=? ($vecN 5) ($vecN K)) (if-template (eq? '$ 'b) B A)))))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (run-all-tests))
