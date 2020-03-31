#lang racket/base

(require glm/vector)

(module+ test
  (require glm/private/testing glm/scalar template (for-syntax racket/base))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-tests "destructive"
    (for/template ([T '(boolean double float int uint)]
                   [$ '(b d i u)])
      (test-suite "T"
        (for/template ([N '(1 2 3 4)])

          ;; Assignment

          (test-suite "$vecN=!"
            (test-case "$vecN=$vecN!"
              (define v1 ($vecN))
              (define v2 ($vecN))
              (define v3 (apply $vecN (for/list ([A '(-1 0 1 2)] [_ N]) A)))
              ($vecN=$vecN! v1 v3)
              ($vecN=! v2 v3)
              (for/template ([X '(x y z w)] [_ N])
                (check = ($vecN-X v1) ($vecN-X v3))
                (check = ($vecN-X v2) ($vecN-X v3))))
            (test-case "$vecN=tvecN!"
              (with-template ([% (if-template (eq? '$ 'd) u d)])
                (define v1 ($vecN))
                (define v2 ($vecN))
                (define v3 (apply %vecN (for/list ([A '(-1 0 1 2)] [_ N]) A)))
                ($vecN=tvecN! v1 v3)
                ($vecN=! v2 v3)
                (for/template ([X '(x y z w)] [_ N])
                  (check = ($vecN-X v1) ($scalar (%vecN-X v3)))
                  (check = ($vecN-X v2) ($scalar (%vecN-X v3)))))))

          ;; In-Place Operations

          (for/template ([□ '(+ - * /)])
            (test-suite "$vecN□=!"
              (test-case "$vecN□=$scalar!"
                (for ([a '(1 2 3)])
                  (define v1 ($vecN 1))
                  (define v2 ($vecN 1))
                  ($vecN□=$scalar! v1 ($scalar a))
                  ($vecN□=! v2 ($scalar a))
                  (for/template ([X '(x y z w)] [_ N])
                    (check = ($vecN-X v1) ($scalar (□ ($scalar 1) ($scalar a))))
                    (check = ($vecN-X v2) ($scalar (□ ($scalar 1) ($scalar a)))))))
              (test-case "$vecN□=$vecN!"
                (define v1 ($vecN 1))
                (define v2 ($vecN 1))
                (define v3 ($vecN (apply $vecN (for/list ([A '(1 2 3 4)] [_ N]) A))))
                ($vecN□=$vecN! v1 v3)
                ($vecN□=! v2 v3)
                (for/template ([X '(x y z w)] [K N])
                  (check = ($vecN-X v1) ($scalar (□ ($scalar 1) ($scalar ($vecN-X v3)))))
                  (check = ($vecN-X v2) ($scalar (□ ($scalar 1) ($scalar ($vecN-X v3)))))))
              ;; (test-case "$vecN□=tvec1!")
              ;; (test-case "$vecN□=tvecN!")
              ))
          
          ))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (run-all-tests))
