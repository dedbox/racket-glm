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

    (define M (mat4 2.0))
    (define N (mat4 1.0))
    (define U (vec4 2.0))

    (check equal? (mat* N 2.0) M)
    (check equal? (mat/ M 2.0) N)
    (check equal? (mat* M U) (vec4 4.0))
    ;; (check equal? (mat/ U M) (vec4 1.0))
    (check equal? (mat* M N) (mat4 2.0))

    ;; (define m (mat4 0.0 0.1 0.2 0.3
    ;;                 1.0 1.1 1.2 1.3
    ;;                 2.0 2.1 2.2 2.3
    ;;                 3.0 3.1 3.2 3.3))

    ;; (mat* m m)

    )

  (test-case "test_inverse"
    (define identity (mat4 1.0))
    (define matrix (mat4 (vec4 0.6 0.2 0.3 0.4)
                         (vec4 0.2 0.7 0.5 0.3)
                         (vec4 0.3 0.5 0.7 0.2)
                         (vec4 0.4 0.3 0.2 0.6)))
    (check equal? identity (mat* (mat/ identity matrix) identity)))

  )
