#lang racket/base

;;; ----------------------------------------------------------------------------
;;; 3-D Vectors
;;;
;;; glm/detail/type_vec3.hpp
;;; glm/detail/type_vec3.inl

(require glm/vec
         racket/generic)

(provide (all-defined-out))

(struct vec3 glm:vec ()
  #:name glm:vec3
  #:constructor-name make-vec3
  #:methods gen:glm-vec
  [(define (vec-name _) 'vec3)
   (define (vec-predicate _) vec3?)
   (define (vec-constructor _) vec3)])

(define vec3
  (case-lambda
    [() (vec3 0.0)]
    [(a) (if (vec3? a) (vec-copy a) (vec3 a a a))]
    [(x y z) (make-vec3 (make-vec-data x y z))]))

(define x-dir3 (vec3 1.0 0.0 0.0))
(define y-dir3 (vec3 0.0 1.0 0.0))
(define z-dir3 (vec3 0.0 0.0 1.0))
(define w-dir3 (vec3 0.0 0.0 0.0))

(define  zero3 (vec3 0.0 0.0 0.0))
(define   one3 (vec3 1.0 1.0 1.0))
(define   two3 (vec3 2.0 2.0 2.0))
(define three3 (vec3 3.0 3.0 3.0))
(define  four3 (vec3 4.0 4.0 4.0))
(define  five3 (vec3 5.0 5.0 5.0))
(define   six3 (vec3 6.0 6.0 6.0))
(define seven3 (vec3 7.0 7.0 7.0))
(define eight3 (vec3 8.0 8.0 8.0))
(define  nine3 (vec3 9.0 9.0 9.0))

(define   -one3 (vec3 -1.0 -1.0 -1.0))
(define   -two3 (vec3 -2.0 -2.0 -2.0))
(define -three3 (vec3 -3.0 -3.0 -3.0))
(define  -four3 (vec3 -4.0 -4.0 -4.0))
(define  -five3 (vec3 -5.0 -5.0 -5.0))
(define   -six3 (vec3 -6.0 -6.0 -6.0))
(define -seven3 (vec3 -7.0 -7.0 -7.0))
(define -eight3 (vec3 -8.0 -8.0 -8.0))
(define  -nine3 (vec3 -9.0 -9.0 -9.0))

;;; ----------------------------------------------------------------------------

(module+ test
  (require racket/function
           rackunit)

  (test-case "equal?"
    (check equal? zero3 zero3)
    (check equal? one3 one3)
    (check (negate equal?) one3 zero3)
    (check (negate equal?) zero3 one3))

  (test-case "vec+="
    (define v (vec3 zero3))
    (check equal? v zero3) (vec+= v one3)
    (check equal? v one3) (vec+= v one3)
    (check equal? v two3))

  (test-case "vec-="
    (define v (vec3 one3))
    (check equal? v one3) (vec-= v one3)
    (check equal? v zero3) (vec-= v one3)
    (check equal? v -one3))

  (test-case "vec*="
    (define v (vec3 one3))
    (check equal? v one3) (vec*= v one3)
    (check equal? v one3) (vec*= v two3)
    (check equal? v two3) (vec*= v two3)
    (check equal? v four3))

  (test-case "vec/="
    (define v (vec3 one3))
    (check equal? v (vec3 1.0)) (vec/= v two3)
    (check equal? v (vec3 0.5)) (vec/= v five3)
    (check equal? v (vec3 0.1)))

  (test-case "vec+"
    (check equal? (vec+ one3) one3)
    (check equal? (vec+ one3 one3) two3)
    (check equal? (vec+ one3 one3 one3) three3))

  (test-case "vec-"
    (check equal? (vec- zero3) zero3)
    (check equal? (vec- one3 zero3) one3)
    (check equal? (vec- one3) -one3)
    (check equal? (vec- one3 one3) zero3)
    (check equal? (vec- one3 one3 one3) -one3)
    (check equal? (vec- one3 one3 one3 one3) -two3))

  (test-case "vec*"
    (check equal? (vec* zero3) zero3)
    (check equal? (vec* one3) one3)
    (check equal? (vec* two3) two3)
    (check equal? (vec* two3 three3) six3)
    (check equal? (vec* two3 three3 four3) (vec3 24.0 24.0 24.0))
    (check equal? (vec* (vec3 1.0 2.0 3.0)
                        (vec3 2.0 3.0 4.0)
                        (vec3 3.0 4.0 5.0)) (vec3 6.0 24.0 60.0)))

  (test-case "vec/"
    (check equal? (vec/ one3) one3)
    (check equal? (vec/ zero3 one3) zero3)
    (check equal? (vec/ one3 two3) (vec3 0.5 0.5 0.5))
    (check equal? (vec/ one3 two3 two3) (vec3 0.25 0.25 0.25))
    (check equal? (vec/ one3 two3 two3 two3) (vec3 0.125 0.125 0.125))))
