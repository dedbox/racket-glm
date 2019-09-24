#lang racket/base

;;; ----------------------------------------------------------------------------
;;; Tests Ported from C++
;;;
;;; test/core/core_type_mat4x4.cpp

(require glm)

(module+ test
  (require rackunit)

  (test-case "test_operators"
    ;; (define ε 0.001)

    (define M (mat4 2))
    (define N (mat4 1))
    (define U (vec4 2))

    (check equal? (* N 2) M)
    (check equal? (/ M 2) N)
    (check equal? (* M U) (vec4 4))
    (check equal? (/ U M) (vec4 1))
    (check equal? (* M N) (mat4 2))

    ;; (define m (mat4 0.0 0.1 0.2 0.3
    ;;                 1.0 1.1 1.2 1.3
    ;;                 2.0 2.1 2.2 2.3
    ;;                 3.0 3.1 3.2 3.3))

    ;; (mat* m m)

    )

  (test-case "test_inverse"
    (define id (mat4 1))
    (define m (mat4 (vec4 0.6 0.2 0.3 0.4)
                         (vec4 0.2 0.7 0.5 0.3)
                         (vec4 0.3 0.5 0.7 0.2)
                         (vec4 0.4 0.3 0.2 0.6)))
    (check equal? m (* (/ m id) id))))
