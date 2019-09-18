#lang racket/base

;;; ----------------------------------------------------------------------------
;;; 4-D Vectors
;;;
;;; glm/detail/type_vec4.hpp
;;; glm/detail/type_vec4.inl

(require glm/vec
         glm/vec3
         racket/function
         racket/generic)

(provide (all-defined-out))

(struct vec4 glm:vec ()
  #:name glm:vec4
  #:constructor-name make-vec4
  #:methods gen:glm-vec
  [(define (vec-name _) 'vec4)
   (define (vec-predicate _) vec4?)
   (define (vec-constructor _) vec4)])

(define vec4
  (case-lambda
    [() (vec4 0.0)]
    [(a) (if (vec4? a) (vec-copy a) (vec4 a a a a))]
    [(a b) (cond [(vec3? a) (apply (curryr vec4 b) (vec->list a))]
                 [(vec3? b) (apply (curry vec4 a) (vec->list b))]
                 [else (raise-argument-error 'vec4 "a scalar or vec3" 0 a b)])]
    [(x y z w) (make-vec4 (make-vec-data x y z w))]))

(define x-dir4 (vec4 1.0 0.0 0.0 0.0))
(define y-dir4 (vec4 0.0 1.0 0.0 0.0))
(define z-dir4 (vec4 0.0 0.0 1.0 0.0))
(define w-dir4 (vec4 0.0 0.0 0.0 1.0))

(define-values (zero4 one4 two4 three4 four4 five4 six4 seven4 eight4 nine4)
  (apply values (for/list ([k (in-range 10)]) (vec4 (real->single-flonum k)))))

(define-values (-one4 -two4 -three4 -four4 -five4 -six4 -seven4 -eight4 -nine4)
  (apply values (for/list ([k (in-range 1 10)]) (vec4 (- (real->single-flonum k))))))

;;; ----------------------------------------------------------------------------

(module+ test
  (require racket/function
           rackunit)

  ;; prototyping tests

  (test-case "equal?"
    (check equal? zero4 zero4)
    (check equal? one4 one4)
    (check (negate equal?) one4 zero4)
    (check (negate equal?) zero4 one4))

  (test-case "vec+="
    (define v (vec4 zero4))
    (check equal? v zero4) (vec+= v one4)
    (check equal? v one4) (vec+= v one4)
    (check equal? v two4))

  (test-case "vec-="
    (define v (vec4 one4))
    (check equal? v one4) (vec-= v one4)
    (check equal? v zero4) (vec-= v one4)
    (check equal? v -one4))

  (test-case "vec*="
    (define v (vec4 one4))
    (check equal? v one4) (vec*= v one4)
    (check equal? v one4) (vec*= v two4)
    (check equal? v two4) (vec*= v two4)
    (check equal? v four4))

  (test-case "vec/="
    (define v (vec4 one4))
    (check equal? v (vec4 1.0)) (vec/= v two4)
    (check equal? v (vec4 0.5)) (vec/= v five4)
    (check equal? v (vec4 0.1)))

  (test-case "vec+"
    (check equal? (vec+ one4) one4)
    (check equal? (vec+ one4 one4) two4)
    (check equal? (vec+ one4 one4 one4) three4))

  (test-case "vec-"
    (check equal? (vec- zero4) zero4)
    (check equal? (vec- one4 zero4) one4)
    (check equal? (vec- one4) -one4)
    (check equal? (vec- one4 one4) zero4)
    (check equal? (vec- one4 one4 one4) -one4)
    (check equal? (vec- one4 one4 one4 one4) -two4))

  (test-case "vec*"
    (check equal? (vec* zero4) zero4)
    (check equal? (vec* one4) one4)
    (check equal? (vec* two4) two4)
    (check equal? (vec* two4 three4) six4)
    (check equal? (vec* two4 three4 four4) (vec4 24.0 24.0 24.0 24.0))
    (check equal? (vec* (vec4 1.0 2.0 3.0 4.0)
                        (vec4 2.0 3.0 4.0 5.0)
                        (vec4 3.0 4.0 5.0 6.0)) (vec4 6.0 24.0 60.0 120.0)))

  (test-case "vec/"
    (check equal? (vec/ one4) one4)
    (check equal? (vec/ zero4 one4) zero4)
    (check equal? (vec/ one4 two4) (vec4 0.5 0.5 0.5 0.5))
    (check equal? (vec/ one4 two4 two4) (vec4 0.25 0.25 0.25 0.25))
    (check equal? (vec/ one4 two4 two4 two4) (vec4 0.125 0.125 0.125 0.125)))

  (test-case "vec++"
    (define v (vec4 zero4))
    (check equal? v zero4) (vec++ v)
    (check equal? v one4) (vec++ v)
    (check equal? v two4))

  (test-case "vec--"
    (define v (vec4 zero4))
    (check equal? v zero4) (vec-- v)
    (check equal? v -one4) (vec-- v)
    (check equal? v -two4))

  (test-case "vec+"
    (check equal? zero4 (vec+ zero4))
    (check equal? one4 (vec+ one4))
    (check equal? two4 (vec+ one4 1.0))
    (check equal? two4 (vec+ one4 one4))
    (check equal? three4 (vec+ one4 1.0 one4))
    (check equal? three4 (vec+ one4 two4)))

  (test-case "vec-"
    (check equal? zero4 (vec- zero4))
    (check equal? -one4 (vec- one4))
    (check equal? zero4 (vec- one4 1.0))
    (check equal? zero4 (vec- one4 one4))
    (check equal? -one4 (vec- one4 1.0 one4))
    (check equal? -one4 (vec- one4 two4)))

  (test-case "vec*"
    (check equal? zero4 (vec* zero4))
    (check equal? zero4 (vec* zero4 0.0))
    (check equal? zero4 (vec* zero4 zero4))
    (check equal? four4 (vec* two4 2.0))
    (check equal?  six4 (vec* two4 three4))
    (check equal?  six4 (vec* one4 2.0 three4)))

  (test-case "vec/"
    (check equal? one4 (vec/ one4))
    (check equal? two4 (vec/ six4 3.0))
    (check equal? two4 (vec/ six4 three4))
    (check equal? one4 (vec/ six4 3.0 two4))))
