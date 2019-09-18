#lang racket/base

;;; ----------------------------------------------------------------------------
;;; Tests Ported from C++
;;;
;;; test/core/core_type_vec4.cpp

(require glm)

(module+ test
  (require rackunit)

  (test-case "test_vec4_ctor"
    (define A (vec4 1.0 2.0 3.0 4.0))
    (define B (vec4 A))
    (check equal? A B)

    (set! A (vec4 1.0))
    (set! B (vec4 1.0 1.0 1.0 1.0))
    (check equal? A B)

    (for ([v (list (vec4 (vec3 1.0 2.0 3.0) 4.0)
                   (vec4 1.0 (vec3 2.0 3.0 4.0))
                   (vec4 1.0 2.0 3.0 4.0)
                   (vec4 (vec4 1.0 2.0 3.0 4.0)))])
      (check equal? v (vec4 1.0 2.0 3.0 4.0))))

  (test-case "test-operators"
    (define A (vec4 1.0 2.0 3.0 4.0))
    (define B (vec4 4.0 5.0 6.0 7.0))

    (check equal? (vec+ A B) (vec4 5.0  7.0  9.0 11.0))
    (check equal? (vec- B A) (vec4 3.0  3.0  3.0  3.0))
    (check equal? (vec* A B) (vec4 4.0 10.0 18.0 28.0))
    (check equal? (vec/ B A) (vec4 4.0  2.5  2.0 (/ 7.0 4.0)))
    (check equal? (vec+ A 1.0) (vec4 2.0 3.0 4.0 5.0))
    (check equal? (vec- B 1.0) (vec4 3.0 4.0 5.0 6.0))
    (check equal? (vec* A 2.0) (vec4 2.0 4.0 6.0 8.0))
    (check equal? (vec/ B 2.0) (vec4 2.0 2.5 3.0 3.5))
    (check equal? (vec+ 1.0 A) (vec4  2.0  3.0  4.0  5.0))
    (check equal? (vec- 1.0 B) (vec4 -3.0 -4.0 -5.0 -6.0))
    (check equal? (vec* 2.0 A) (vec4  2.0  4.0  6.0  8.0))
    (check equal? (vec/ 2.0 B) (vec4 0.5 (/ 2.0 5.0) (/ 2.0 6.0) (/ 2.0 7.0)))

    (vec+= A B)   (check equal? A (vec4 5.0 7.0 9.0 11.0))
    (vec+= A 1.0) (check equal? A (vec4 6.0 8.0 10.0 12.0))

    (set! A (vec4 1.0 2.0 3.0 4.0))

    (vec*= A B)   (check equal? A (vec4 4.0 10.0 18.0 28.0))
    (vec*= A 2.0) (check equal? A (vec4 8.0 20.0 36.0 56.0))

    (set! A (vec4 1.0 2.0 2.0 4.0))
    (set! B (vec4 4.0 4.0 8.0 8.0))

    (vec/= B A)   (check equal? B (vec4 4.0 2.0 4.0 2.0))
    (vec/= B 2.0) (check equal? B (vec4 2.0 1.0 2.0 1.0))

    (set! B (vec4 2.0))
    (vec/= B (vec-ref B 0))
    (check equal? B (vec4 1.0))

    (set! A (vec4 1.0 2.0 3.0 4.0))
    (set! B (vec- A))
    (check equal? B (vec4 -1.0 -2.0 -3.0 -4.0))

    (set! B (--vec A))
    (check equal? A (vec4 0.0 1.0 2.0 3.0))

    (set! A (vec4 1.0 2.0 3.0 4.0))
    (set! B (vec-- A))
    (check equal? B (vec4 1.0 2.0 3.0 4.0))
    (check equal? A (vec4 0.0 1.0 2.0 3.0))

    (set! A (vec4 1.0 2.0 3.0 4.0))
    (set! B (++vec A))
    (check equal? B (vec4 2.0 3.0 4.0 5.0))

    (set! A (vec4 1.0 2.0 3.0 4.0))
    (set! B (vec++ A))
    (check equal? B (vec4 1.0 2.0 3.0 4.0))
    (check equal? A (vec4 2.0 3.0 4.0 5.0))))
