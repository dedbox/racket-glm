#lang racket/base

(require glm/vector)

(module+ test
  (require glm/private/testing glm/scalar template (for-syntax racket/base))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-tests "accessors"
    (for/template ([T '(boolean double float int uint)]
                   [$ '(b d || i u)])
      (test-suite "T"
        (for/template ([N '(1 2 3 4)])
          (test-case "$vecN"
            (define v ($vecN ($vec4 -1 0 1 2)))
            (for/template ([X '(x y z w)]
                           [R '(r g b a)]
                           [S '(s t p q)]
                           [K N])
              (check = ($vecN-X v) ($scalar (- K 1)))
              (check = ($vecN-R v) ($scalar (- K 1)))
              (check = ($vecN-S v) ($scalar (- K 1)))))))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (run-all-tests))
