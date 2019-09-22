#lang racket/base

;;; ----------------------------------------------------------------------------
;;; Tests Ported from C++
;;;
;;; test/ext/ext_matrix_transform.cpp

(require glm)

(module+ test
  (require rackunit
           racket/math)

  (test-case "test_translate"
    (check equal? (list-ref (mat-columns (translate (mat4 1.0) (vec3 1.0))) 3)
           (vec4 1.0)))

  (test-case "test_scale"
    (check equal? (scale (mat4 1.0) (vec3 2.0))
           (mat4 (vec4 2.0 0.0 0.0 0.0)
                 (vec4 0.0 2.0 0.0 0.0)
                 (vec4 0.0 0.0 2.0 0.0)
                 (vec4 0.0 0.0 0.0 1.0))))

  (test-case "test_rotate"
    (define A (vec4 1.0 0.0 0.0 1.0))
    (define R (rotate (mat4 1.0) (radians 90.0) (vec3 0.0 0.0 1.0)))
    (check equal? (mat* R A)
           (vec4 6.123234262925839e-17 -1.0 0.0 1.0))))
