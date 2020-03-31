#lang racket/base

(require glm/vector)

(module+ test
  (require glm/private/testing glm/scalar template (for-syntax racket/base))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-tests "mutators"
    (for/template ([T '(boolean double float int uint)]
                   [$ '(b d i u)])
      (test-suite "T"
        (for/template ([N '(1 2 3 4)])
          (test-suite "$vecN"
            (for/template ([Cs '((x y z w)
                                 (r g b a)
                                 (s t p q))])
              (test-case (apply string-append (map symbol->string 'Cs))
                (define v ($vecN))
                (for/template ([C 'Cs] [K N])
                  (set-$vecN-C! v ($scalar (- K 1))))
                (for/template ([C 'Cs] [K N])
                  (check = ($vecN-C v) ($scalar (- K 1)))))))))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (run-all-tests))
