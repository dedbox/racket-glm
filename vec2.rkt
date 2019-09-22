#lang racket/base

;;; ----------------------------------------------------------------------------
;;; 2-D Vectors
;;;
;;; glm/detail/type_vec2.hpp
;;; glm/detail/type_vec2.inl

(require glm/vec
         racket/generic)

(provide (all-defined-out))

(struct vec2 glm:vec ()
  #:name glm:vec2
  #:constructor-name make-vec2
  #:methods gen:glm-vec
  [(define (vec-name _) 'vec2)
   (define (vec-predicate _) vec2?)
   (define (vec-constructor _) vec2)])

(define vec2
  (case-lambda
    [() (vec2 0.0)]
    [(a) (if (vec2? a) (vec-copy a) (vec2 a a))]
    [(x y) (make-vec2 (make-vec-data x y))]))

(define x-dir2 (vec2 1.0 0.0))
(define y-dir2 (vec2 0.0 1.0))
(define z-dir2 (vec2 0.0 0.0))
(define w-dir2 (vec2 0.0 0.0))

(define  zero2 (vec2 0.0 0.0))
(define   one2 (vec2 1.0 1.0))
(define   two2 (vec2 2.0 2.0))
(define three2 (vec2 3.0 3.0))
(define  four2 (vec2 4.0 4.0))
(define  five2 (vec2 5.0 5.0))
(define   six2 (vec2 6.0 6.0))
(define seven2 (vec2 7.0 7.0))
(define eight2 (vec2 8.0 8.0))
(define  nine2 (vec2 9.0 9.0))

(define   -one2 (vec2 -1.0 -1.0))
(define   -two2 (vec2 -2.0 -2.0))
(define -three2 (vec2 -3.0 -3.0))
(define  -four2 (vec2 -4.0 -4.0))
(define  -five2 (vec2 -5.0 -5.0))
(define   -six2 (vec2 -6.0 -6.0))
(define -seven2 (vec2 -7.0 -7.0))
(define -eight2 (vec2 -8.0 -8.0))
(define  -nine2 (vec2 -9.0 -9.0))

;;; ----------------------------------------------------------------------------

(module+ test
  (require racket/function
           rackunit)

  (test-case "equal?"
    (check equal? zero2 zero2)
    (check equal? one2 one2)
    (check (negate equal?) one2 zero2)
    (check (negate equal?) zero2 one2))

  (test-case "vec+="
    (define v (vec2 zero2))
    (check equal? v zero2) (vec+= v one2)
    (check equal? v one2) (vec+= v one2)
    (check equal? v two2))

  (test-case "vec-="
    (define v (vec2 one2))
    (check equal? v one2) (vec-= v one2)
    (check equal? v zero2) (vec-= v one2)
    (check equal? v -one2))

  (test-case "vec*="
    (define v (vec2 one2))
    (check equal? v one2) (vec*= v one2)
    (check equal? v one2) (vec*= v two2)
    (check equal? v two2) (vec*= v two2)
    (check equal? v four2))

  (test-case "vec/="
    (define v (vec2 one2))
    (check equal? v (vec2 1.0)) (vec/= v two2)
    (check equal? v (vec2 0.5)) (vec/= v five2)
    (check equal? v (vec2 0.1)))

  (test-case "vec+"
    (check equal? (vec+ one2) one2)
    (check equal? (vec+ one2 one2) two2)
    (check equal? (vec+ one2 one2 one2) three2))

  (test-case "vec-"
    (check equal? (vec- zero2) zero2)
    (check equal? (vec- one2 zero2) one2)
    (check equal? (vec- one2) -one2)
    (check equal? (vec- one2 one2) zero2)
    (check equal? (vec- one2 one2 one2) -one2)
    (check equal? (vec- one2 one2 one2 one2) -two2))

  (test-case "vec*"
    (check equal? (vec* zero2) zero2)
    (check equal? (vec* one2) one2)
    (check equal? (vec* two2) two2)
    (check equal? (vec* two2 three2) six2)
    (check equal? (vec* two2 three2 four2) (vec2 24.0 24.0))
    (check equal? (vec* (vec2 1.0 2.0)
                        (vec2 2.0 3.0)
                        (vec2 3.0 4.0)) (vec2 6.0 24.0)))

  (test-case "vec/"
    (check equal? (vec/ one2) one2)
    (check equal? (vec/ zero2 one2) zero2)
    (check equal? (vec/ one2 two2) (vec2 0.5 0.5))
    (check equal? (vec/ one2 two2 two2) (vec2 0.25 0.25))
    (check equal? (vec/ one2 two2 two2 two2) (vec2 0.125 0.125))))
