#lang racket/base

(require glm/scalar
         glm/vector)

(module+ test
  (require rackunit
           rackunit/text-ui
           template
           (for-syntax racket/base))

  (define (run-all-tests)
    (run-tests the-tests)
    (void))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (begin-template
    (define-test-suite the-tests
      (for/template ([$ (in-list '(b d || i u))]) $vec-tests)))

  (for/template ([$ (in-list '(b d || i u))]
                 [R-1 (in-list '(1 -1.0 -1.0 -1 4294967295))]
                 [R0  (in-list '(0  0.0  0.0  0 0))]
                 [R1  (in-list '(1  1.0  1.0  1 1))]
                 [R2  (in-list '(1  2.0  2.0  2 2))]
                 [Rt  (in-list '(1  1.0  1.0  1 1))]
                 [Rf  (in-list '(0  0.0  0.0  0 0))])

    (define-test-suite $vec-tests
      (for/template ([N (in-list '(1 2 3 4))])

        (test-case "$vecN?"
          (check-false ($vecN? -1))
          (for/template ([M (in-list '(1 2 3 4))])
            ((if-template (= M N) check-true check-false) ($vecN? ($vecM)))))

        (test-case "($vecN)"
          (define v ($vecN))
          (for/template ([X (in-list '(x y z w))]
                         [R (in-list '(r g b a))]
                         [S (in-list '(s t p q))]
                         [_ (in-range N)])
            (check = ($vecN-X v) R0)
            (check = ($vecN-R v) R0)
            (check = ($vecN-S v) R0)))

        (test-case "($vecN -1)"
          (define v ($vecN -1))
          (for/template ([X (in-list '(x y z w))]
                         [R (in-list '(r g b a))]
                         [S (in-list '(s t p q))]
                         [_ (in-range N)])
            (check = ($vecN-X v) R-1)
            (check = ($vecN-R v) R-1)
            (check = ($vecN-S v) R-1)))

        (test-case "($vecN 0)"
          (define v ($vecN 0))
          (for/template ([X (in-list '(x y z w))]
                         [R (in-list '(r g b a))]
                         [S (in-list '(s t p q))]
                         [_ (in-range N)])
            (check = ($vecN-X v) R0)
            (check = ($vecN-R v) R0)
            (check = ($vecN-S v) R0)))

        (test-case "($vecN 1)"
          (define v ($vecN 1))
          (for/template ([X (in-list '(x y z w))]
                         [R (in-list '(r g b a))]
                         [S (in-list '(s t p q))]
                         [_ (in-range N)])
            (check = ($vecN-X v) R1)
            (check = ($vecN-R v) R1)
            (check = ($vecN-S v) R1)))

        (test-case "($vecN 2)"
          (define v ($vecN 2))
          (for/template ([X (in-list '(x y z w))]
                         [R (in-list '(r g b a))]
                         [S (in-list '(s t p q))]
                         [_ (in-range N)])
            (check = ($vecN-X v) R2)
            (check = ($vecN-R v) R2)
            (check = ($vecN-S v) R2)))

        (test-case "($vecN #t)"
          (define v ($vecN #t))
          (for/template ([X (in-list '(x y z w))]
                         [R (in-list '(r g b a))]
                         [S (in-list '(s t p q))]
                         [_ (in-range N)])
            (check = ($vecN-X v) Rt)
            (check = ($vecN-R v) Rt)
            (check = ($vecN-S v) Rt)))

        (test-case "($vecN #f)"
          (define v ($vecN #f))
          (for/template ([X (in-list '(x y z w))]
                         [R (in-list '(r g b a))]
                         [S (in-list '(s t p q))]
                         [_ (in-range N)])
            (check = ($vecN-X v) Rf)
            (check = ($vecN-R v) Rf)
            (check = ($vecN-S v) Rf)))

        (test-case "$vecN-ref"
          (define v (apply $vecN (build-list N values)))
          (for/template ([I (in-range N)])
            (check = ($vecN-ref v I) ($scalar I))))

        (test-case "$vecN=!"
          (define v1 ($vecN 0))
          (define v2 ($vecN 0))
          (define v3 ($vecN 0))
          (for/template ([V (in-list '(1 0 2))])
            (check-pred $vecN? ($vecN=! v1 ($vecN V)))
            (check-pred $vecN? ($vecN=$vecN! v2 ($vecN V)))
            (check-pred $vecN? ($vecN=tvecN! v3 ((if-template (eq? '$ 'i) dvecN ivecN) V)))
            (for/template ([X (in-list '(x y z w))]
                           [_ (in-range N)])
              (check = ($vecN-X v1) ($scalar V))
              (check = ($vecN-X v2) ($scalar V))
              (check = ($vecN-X v3)
                     ($scalar ((if-template (eq? '$ 'i) dscalar iscalar) V))))))

        (for/template ([⊙ (in-list '(+ - * /))]
                       [As (in-list '((2  4  7)
                                      (0 -2 -5)
                                      (1  2  6)
                                      (1 1/2 1/6)))]
                       [Bs (in-list '((1 1 1)
                                      (0 1 0)
                                      (1 1 1)
                                      (1 1 1)))])
          (test-case "$vecN⊙=!"
            (define v ($vecN 1))
            (for/template ([V (in-list '(1 2 3))]
                           [A (in-list 'As)]
                           [B (in-list 'Bs)])
              (check-pred $vecN? ($vecN⊙=! v ($vecN V)))
              (for/template ([X (in-list '(x y z w))]
                             [_ (in-range N)])
                (check = ($vecN-X v) ($scalar (if-template (eq? '$ 'b) B A)))))))

        (for/template ([⊙ (in-list '(+ - * /))]
                       [A (in-list '(2 0 1 1))]
                       [B (in-list '(1 0 1 1))])
          (test-case "$vecN⊙"
            (define v ($vecN⊙ ($vecN 1) ($vecN 1)))
            (for/template ([X (in-list '(x y z w))]
                           [_ (in-range N)])
              (check = ($vecN-X v) ($scalar (if-template (eq? '$ 'b) B A))))))

        (test-case "$vecN++!"
          (define v ($vecN))
          (check-pred $vecN? ($vecN++! v))
          (for/template ([X (in-list '(x y z w))]
                         [_ (in-range N)])
            (check = ($vecN-X v) ($scalar 1)))
          (check-pred $vecN? ($vecN++! v))
          (for/template ([X (in-list '(x y z w))]
                         [_ (in-range N)])
            (check = ($vecN-X v) ($scalar (if-template (eq? '$ 'b) 1 2)))))

        (test-case "$vecN!--"
          (define v ($vecN 1))
          (check-pred $vecN? ($vecN--! v))
          (for/template ([X (in-list '(x y z w))]
                         [_ (in-range N)])
            (check = ($vecN-X v) ($scalar 0)))
          (check-pred $vecN? ($vecN--! v))
          (for/template ([X (in-list '(x y z w))]
                         [_ (in-range N)])
            (check = ($vecN-X v) ($scalar (if-template (eq? '$ 'b) 1 -1)))))

        (test-case "$vecN%=!"
          (for/template ([K (in-list '(2 3 4))]
                         [A (in-list '(0 1 2))]
                         [B (in-list '(0 0 0))])
            (let ([v ($vecN 10)])
              (check-pred $vecN? ($vecN%=! v K))
              (for/template ([X (in-list '(x y z w))]
                             [_ (in-range N)])
                (check = ($vecN-X v) ($scalar (if-template (eq? '$ 'b) B A)))))))

        (test-case "$vecN&=!"
          (for/template ([K (in-list '(36 34 32))]
                         [A (in-list '( 4  2  0))]
                         [B (in-list '( 1  1  1))])
            (let ([v ($vecN 15)])
              (check-pred $vecN? ($vecN&=! v K))
              (for/template ([X (in-list '(x y z w))]
                             [_ (in-range N)])
                (check = ($vecN-X v) ($scalar (if-template (eq? '$ 'b) B A)))))))

        (test-case "$vecN//=!"
          (for/template ([K (in-list '(15 16 32))]
                         [A (in-list '(15 31 47))]
                         [B (in-list '( 1  1  1))])
            (let ([v ($vecN 15)])
              (check-pred $vecN? ($vecN//=! v K))
              (for/template ([X (in-list '(x y z w))]
                             [_ (in-range N)])
                (check = ($vecN-X v) ($scalar (if-template (eq? '$ 'b) B A)))))))

        (test-case "$vecN^=!"
          (for/template ([K (in-list '(0 1 2 3))]
                         [A (in-list '(3 2 1 0))]
                         [B (in-list '(1 0 0 0))])
            (let ([v ($vecN 3)])
              (check-pred $vecN? ($vecN^=! v K))
              (for/template ([X (in-list '(x y z w))]
                             [_ (in-range N)])
                (check = ($vecN-X v) ($scalar (if-template (eq? '$ 'b) B A)))))))

        (test-case "$vecN<<=!"
          (for/template ([K (in-list '(0 1 2 3))]
                         [A (in-list '(1 2 4 8))]
                         [B (in-list '(1 1 1 1))])
            (let ([v ($vecN 1)])
              (check-pred $vecN? ($vecN<<=! v K))
              (for/template ([X (in-list '(x y z w))]
                             [_ (in-range N)])
                (check = ($vecN-X v) ($scalar (if-template (eq? '$ 'b) B A)))))))

        (test-case "$vecN>>=!"
          (for/template ([K (in-list '(0 1 2 3 4))]
                         [A (in-list '(8 4 2 1 0))]
                         [B (in-list '(1 0 0 0 0))])
            (let ([v ($vecN 8)])
              (check-pred $vecN? ($vecN>>=! v K))
              (for/template ([X (in-list '(x y z w))]
                             [_ (in-range N)])
                (check = ($vecN-X v) ($scalar (if-template (eq? '$ 'b) B A)))))))

        (test-case "$vecN%"
          (for/template ([K (in-list '(2 3 4))]
                         [A (in-list '(0 1 2))]
                         [B (in-list '(0 0 0))])
            (let ([v ($vecN% ($vecN 10) K)])
              (check-pred $vecN? v)
              (for/template ([X (in-list '(x y z w))]
                             [_ (in-range N)])
                (check = ($vecN-X v) ($scalar (if-template (eq? '$ 'b) B A)))))))

        (test-case "$vecN&"
          (for/template ([K (in-list '(36 34 32))]
                         [A (in-list '( 4  2  0))]
                         [B (in-list '( 1  1  1))])
            (let ([v ($vecN& ($vecN 15) K)])
              (check-pred $vecN? v)
              (for/template ([X (in-list '(x y z w))]
                             [_ (in-range N)])
                (check = ($vecN-X v) ($scalar (if-template (eq? '$ 'b) B A)))))))

        (test-case "$vecN//"
          (for/template ([K (in-list '(15 16 32))]
                         [A (in-list '(15 31 47))]
                         [B (in-list '( 1  1  1))])
            (let ([v ($vecN// ($vecN 15) K)])
              (check-pred $vecN? v)
              (for/template ([X (in-list '(x y z w))]
                             [_ (in-range N)])
                (check = ($vecN-X v) ($scalar (if-template (eq? '$ 'b) B A)))))))

        (test-case "$vecN^"
          (for/template ([K (in-list '(0 1 2 3))]
                         [A (in-list '(3 2 1 0))]
                         [B (in-list '(1 0 0 0))])
            (let ([v ($vecN^ ($vecN 3) K)])
              (check-pred $vecN? v)
              (for/template ([X (in-list '(x y z w))]
                             [_ (in-range N)])
                (check = ($vecN-X v) ($scalar (if-template (eq? '$ 'b) B A)))))))

        (test-case "$vecN<<"
          (for/template ([K (in-list '(0 1 2 3))]
                         [A (in-list '(1 2 4 8))]
                         [B (in-list '(1 1 1 1))])
            (let ([v ($vecN<< ($vecN 1) K)])
              (check-pred $vecN? v)
              (for/template ([X (in-list '(x y z w))]
                             [_ (in-range N)])
                (check = ($vecN-X v) ($scalar (if-template (eq? '$ 'b) B A)))))))

        (test-case "$vecN>>"
          (for/template ([K (in-list '(0 1 2 3 4))]
                         [A (in-list '(8 4 2 1 0))]
                         [B (in-list '(1 0 0 0 0))])
            (let ([v ($vecN>> ($vecN 8) K)])
              (check-pred $vecN? v)
              (for/template ([X (in-list '(x y z w))]
                             [_ (in-range N)])
                (check = ($vecN-X v) ($scalar (if-template (eq? '$ 'b) B A)))))))

        (test-case "$vecN~"
          (for/template ([K (in-list '(         0          1          2))]
                         [A (in-list '(        -1         -2         -3))]
                         [U (in-list '(#xFFFFFFFF #xFFFFFFFE #xFFFFFFFD))]
                         [B (in-list '(         1          1          1))])
            (let ([v ($vecN~ ($vecN K))])
              (check-pred $vecN? v)
              (for/template ([X (in-list '(x y z w))]
                             [_ (in-range N)])
                (check = ($vecN-X v) ($scalar (if-template (eq? '$ 'b) B A)))))))

        (test-case "$vecN=?"
          (for/template ([K (in-list '( 4  5  6))]
                         [A (in-list '(#f #t #f))]
                         [B (in-list '(#t #t #t))])
            (check eq? ($vecN=? ($vecN 5) ($vecN K)) (if-template (eq? '$ 'b) B A)))))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (run-all-tests))
