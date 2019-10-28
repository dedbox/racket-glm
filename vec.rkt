#lang racket/base

(require ffi/unsafe
         ffi/vector
         glm/private/vector)

(provide (all-defined-out))

(define-numeric-vector-type vec _float real? single-flonum?
  #:from-scalar real->single-flonum
  #:to-scalar (λ (a) (real->single-flonum (if (exact? a) (exact->inexact a) a)))
  #:to-native (λ (x) (+ 0.0 x))
  #:ffi f32vector)

;;; ============================================================================

;; (module+ test
;;   (require racket/function
;;            rackunit)

;;   (test-case "equal?"
;;     (check equal? (vec2 0) (vec2 0))
;;     (check equal? (vec2 1) (vec2 1))
;;     (check (negate equal?) (vec2 1) (vec2 0))
;;     (check (negate equal?) (vec2 0) (vec2 1)))

;;   (test-case "vec+="
;;     (define v
;;       (vec2 (vec2 0))) (check equal? v (vec2 0))
;;     (vec+= v (vec2 1)) (check equal? v (vec2 1))
;;     (vec+= v (vec2 1)) (check equal? v (vec2 2)))

;;   (test-case "vec-="
;;     (define v
;;       (vec2 (vec2 1))) (check equal? v (vec2 1))
;;     (vec-= v (vec2 1)) (check equal? v (vec2 0))
;;     (vec-= v (vec2 1)) (check equal? v (vec2 -1)))

;;   (test-case "vec*="
;;     (define v
;;       (vec2 (vec2 1))) (check equal? v (vec2 1))
;;     (vec*= v (vec2 1)) (check equal? v (vec2 1))
;;     (vec*= v (vec2 2)) (check equal? v (vec2 2))
;;     (vec*= v (vec2 2)) (check equal? v (vec2 4)))

;;   (test-case "vec/="
;;     (define v
;;       (vec2 (vec2 1))) (check equal? v (vec2  1 ))
;;     (vec/= v (vec2 2)) (check equal? v (vec2 1/2))
;;     (vec/= v (vec2 5)) (check equal? v (vec2 0.1)))

;;   (test-case "vec+"
;;     (check equal? (vec+ (vec2 1)) (vec2 1))
;;     (check equal? (vec+ (vec2 1) (vec2 1)) (vec2 2))
;;     (check equal? (vec+ (vec2 1) (vec2 1) (vec2 1)) (vec2 3)))

;;   (test-case "vec-"
;;     (check equal? (vec- (vec2 0)) (vec2 0))
;;     (check equal? (vec- (vec2 1) (vec2 0)) (vec2 1))
;;     (check equal? (vec- (vec2 1)) (vec2 -1))
;;     (check equal? (vec- (vec2 1) (vec2 1)) (vec2 0))
;;     (check equal? (vec- (vec2 1) (vec2 1) (vec2 1)) (vec2 -1))
;;     (check equal? (vec- (vec2 1) (vec2 1) (vec2 1) (vec2 1)) (vec2 -2)))

;;   (test-case "vec*"
;;     (check equal? (vec* (vec2 0)) (vec2 0))
;;     (check equal? (vec* (vec2 1)) (vec2 1))
;;     (check equal? (vec* (vec2 2)) (vec2 2))
;;     (check equal? (vec* (vec2 2) (vec2 3)) (vec2 6))
;;     (check equal? (vec* (vec2 2) (vec2 3) (vec2 4)) (vec2 24 24))
;;     (check equal? (vec* (vec2 1 2) (vec2 2 3) (vec2 3 4)) (vec2 6 24)))

;;   (test-case "vec/"
;;     (check equal? (vec/ (vec2 1)) (vec2 1))
;;     (check equal? (vec/ (vec2 0) (vec2 1)) (vec2 0))
;;     (check equal? (vec/ (vec2 1) (vec2 2)) (vec2 1/2 1/2))
;;     (check equal? (vec/ (vec2 1) (vec2 2) (vec2 2)) (vec2 1/4 1/4))
;;     (check equal? (vec/ (vec2 1) (vec2 2) (vec2 2) (vec2 2)) (vec2 1/8 1/8)))

;;   ;;; ..........................................................................

;;   (test-case "equal?"
;;     (check equal? (vec2 0) (vec2 0))
;;     (check equal? (vec2 1) (vec2 1))
;;     (check (negate equal?) (vec2 1) (vec2 0))
;;     (check (negate equal?) (vec2 0) (vec2 1)))

;;   (test-case "vec+="
;;     (define v
;;       (vec2 (vec2 0))) (check equal? v (vec2 0))
;;     (vec+= v (vec2 1)) (check equal? v (vec2 1))
;;     (vec+= v (vec2 1)) (check equal? v (vec2 2)))

;;   (test-case "vec-="
;;     (define v
;;       (vec2 (vec2 1))) (check equal? v (vec2 1))
;;     (vec-= v (vec2 1)) (check equal? v (vec2 0))
;;     (vec-= v (vec2 1)) (check equal? v (vec2 -1)))

;;   (test-case "vec*="
;;     (define v
;;       (vec2 (vec2 1))) (check equal? v (vec2 1))
;;     (vec*= v (vec2 1)) (check equal? v (vec2 1))
;;     (vec*= v (vec2 2)) (check equal? v (vec2 2))
;;     (vec*= v (vec2 2)) (check equal? v (vec2 4)))

;;   (test-case "vec/="
;;     (define v (vec2 (vec2 1)))
;;     (check equal? v (vec2  1 )) (vec/= v (vec2 2))
;;     (check equal? v (vec2 1/2)) (vec/= v (vec2 5))
;;     (check equal? v (vec2 0.1)))

;;   (test-case "vec+"
;;     (check equal? (vec+ (vec2 1)) (vec2 1))
;;     (check equal? (vec+ (vec2 1) (vec2 1)) (vec2 2))
;;     (check equal? (vec+ (vec2 1) (vec2 1) (vec2 1)) (vec2 3)))

;;   (test-case "vec-"
;;     (check equal? (vec- (vec2 0)) (vec2 0))
;;     (check equal? (vec- (vec2 1) (vec2 0)) (vec2 1))
;;     (check equal? (vec- (vec2 1)) (vec2 -1))
;;     (check equal? (vec- (vec2 1) (vec2 1)) (vec2 0))
;;     (check equal? (vec- (vec2 1) (vec2 1) (vec2 1)) (vec2 -1))
;;     (check equal? (vec- (vec2 1) (vec2 1) (vec2 1) (vec2 1)) (vec2 -2)))

;;   (test-case "vec*"
;;     (check equal? (vec* (vec2 0)) (vec2 0))
;;     (check equal? (vec* (vec2 1)) (vec2 1))
;;     (check equal? (vec* (vec2 2)) (vec2 2))
;;     (check equal? (vec* (vec2 2) (vec2 3)) (vec2 6))
;;     (check equal? (vec* (vec2 2) (vec2 3) (vec2 4)) (vec2 24))
;;     (check equal? (vec* (vec2 1 2) (vec2 2 3) (vec2 3 4)) (vec2 6 24)))

;;   (test-case "vec/"
;;     (check equal? (vec/ (vec2 1)) (vec2 1))
;;     (check equal? (vec/ (vec2 0) (vec2 1)) (vec2 0))
;;     (check equal? (vec/ (vec2 1) (vec2 2)) (vec2 1/2))
;;     (check equal? (vec/ (vec2 1) (vec2 2) (vec2 2)) (vec2 1/4))
;;     (check equal? (vec/ (vec2 1) (vec2 2) (vec2 2) (vec2 2)) (vec2 1/8)))

;;   ;;; ..........................................................................

;;   (test-case "equal?"
;;     (check equal? (vec3 0) (vec3 0))
;;     (check equal? (vec3 1) (vec3 1))
;;     (check (negate equal?) (vec3 1) (vec3 0))
;;     (check (negate equal?) (vec3 0) (vec3 1)))

;;   (test-case "vec+="
;;     (define v
;;       (vec3 (vec3 0))) (check equal? v (vec3 0))
;;     (vec+= v (vec3 1)) (check equal? v (vec3 1))
;;     (vec+= v (vec3 1)) (check equal? v (vec3 2)))

;;   (test-case "vec-="
;;     (define v
;;       (vec3 (vec3 1))) (check equal? v (vec3 1))
;;     (vec-= v (vec3 1)) (check equal? v (vec3 0))
;;     (vec-= v (vec3 1)) (check equal? v (vec3 -1)))

;;   (test-case "vec*="
;;     (define v
;;       (vec3 (vec3 1))) (check equal? v (vec3 1))
;;     (vec*= v (vec3 1)) (check equal? v (vec3 1))
;;     (vec*= v (vec3 2)) (check equal? v (vec3 2))
;;     (vec*= v (vec3 2)) (check equal? v (vec3 4)))

;;   (test-case "vec/="
;;     (define v
;;       (vec3 (vec3 1))) (check equal? v (vec3  1 ))
;;     (vec/= v (vec3 2)) (check equal? v (vec3 1/2))
;;     (vec/= v (vec3 5)) (check equal? v (vec3 0.1)))

;;   (test-case "vec+"
;;     (check equal? (vec+ (vec3 1)) (vec3 1))
;;     (check equal? (vec+ (vec3 1) (vec3 1)) (vec3 2))
;;     (check equal? (vec+ (vec3 1) (vec3 1) (vec3 1)) (vec3 3)))

;;   (test-case "vec-"
;;     (check equal? (vec- (vec3 0)) (vec3 0))
;;     (check equal? (vec- (vec3 1) (vec3 0)) (vec3 1))
;;     (check equal? (vec- (vec3 1)) (vec3 -1))
;;     (check equal? (vec- (vec3 1) (vec3 1)) (vec3 0))
;;     (check equal? (vec- (vec3 1) (vec3 1) (vec3 1)) (vec3 -1))
;;     (check equal? (vec- (vec3 1) (vec3 1) (vec3 1) (vec3 1)) (vec3 -2)))

;;   (test-case "vec*"
;;     (check equal? (vec* (vec3 0)) (vec3 0))
;;     (check equal? (vec* (vec3 1)) (vec3 1))
;;     (check equal? (vec* (vec3 2)) (vec3 2))
;;     (check equal? (vec* (vec3 2) (vec3 3)) (vec3 6))
;;     (check equal? (vec* (vec3 2) (vec3 3) (vec3 4)) (vec3 24))
;;     (check equal? (vec* (vec3 1 2 3) (vec3 2 3 4) (vec3 3 4 5)) (vec3 6 24 60)))

;;   (test-case "vec/"
;;     (check equal? (vec/ (vec3 1)) (vec3 1))
;;     (check equal? (vec/ (vec3 0) (vec3 1)) (vec3 0))
;;     (check equal? (vec/ (vec3 1) (vec3 2)) (vec3 1/2))
;;     (check equal? (vec/ (vec3 1) (vec3 2) (vec3 2)) (vec3 1/4))
;;     (check equal? (vec/ (vec3 1) (vec3 2) (vec3 2) (vec3 2)) (vec3 1/8)))

;;   ;;; ..........................................................................

;;   (test-case "equal?"
;;     (check equal? (vec4 0) (vec4 0))
;;     (check equal? (vec4 1) (vec4 1))
;;     (check (negate equal?) (vec4 1) (vec4 0))
;;     (check (negate equal?) (vec4 0) (vec4 1)))

;;   (test-case "vec+="
;;     (define v
;;       (vec4 (vec4 0))) (check equal? v (vec4 0))
;;     (vec+= v (vec4 1)) (check equal? v (vec4 1))
;;     (vec+= v (vec4 1)) (check equal? v (vec4 2)))

;;   (test-case "vec-="
;;     (define v
;;       (vec4 (vec4 1))) (check equal? v (vec4 1))
;;     (vec-= v (vec4 1)) (check equal? v (vec4 0))
;;     (vec-= v (vec4 1)) (check equal? v (vec4 -1)))

;;   (test-case "vec*="
;;     (define v
;;       (vec4 (vec4 1))) (check equal? v (vec4 1))
;;     (vec*= v (vec4 1)) (check equal? v (vec4 1))
;;     (vec*= v (vec4 2)) (check equal? v (vec4 2))
;;     (vec*= v (vec4 2)) (check equal? v (vec4 4)))

;;   (test-case "vec/="
;;     (define v
;;       (vec4 (vec4 1))) (check equal? v (vec4 1.0))
;;     (vec/= v (vec4 2)) (check equal? v (vec4 0.5))
;;     (vec/= v (vec4 5)) (check equal? v (vec4 0.1)))

;;   (test-case "vec+"
;;     (check equal? (vec+ (vec4 1)) (vec4 1))
;;     (check equal? (vec+ (vec4 1) (vec4 1)) (vec4 2))
;;     (check equal? (vec+ (vec4 1) (vec4 1) (vec4 1)) (vec4 3)))

;;   (test-case "vec-"
;;     (check equal? (vec- (vec4 0)) (vec4 0))
;;     (check equal? (vec- (vec4 1) (vec4 0)) (vec4 1))
;;     (check equal? (vec- (vec4 1)) (vec4 -1))
;;     (check equal? (vec- (vec4 1) (vec4 1)) (vec4 0))
;;     (check equal? (vec- (vec4 1) (vec4 1) (vec4 1)) (vec4 -1))
;;     (check equal? (vec- (vec4 1) (vec4 1) (vec4 1) (vec4 1)) (vec4 -2)))

;;   (test-case "vec*"
;;     (check equal? (vec* (vec4 0)) (vec4 0))
;;     (check equal? (vec* (vec4 1)) (vec4 1))
;;     (check equal? (vec* (vec4 2)) (vec4 2))
;;     (check equal? (vec* (vec4 2) (vec4 3)) (vec4 6))
;;     (check equal? (vec* (vec4 2) (vec4 3) (vec4 4)) (vec4 24))
;;     (check equal? (vec* (vec4 1 2 3 4)
;;                         (vec4 2 3 4 5)
;;                         (vec4 3 4 5 6)) (vec4 6 24 60 120)))

;;   (test-case "vec/"
;;     (check equal? (vec/ (vec4 1)) (vec4 1))
;;     (check equal? (vec/ (vec4 0) (vec4 1)) (vec4 0))
;;     (check equal? (vec/ (vec4 1) (vec4 2)) (vec4 1/2))
;;     (check equal? (vec/ (vec4 1) (vec4 2) (vec4 2)) (vec4 1/4))
;;     (check equal? (vec/ (vec4 1) (vec4 2) (vec4 2) (vec4 2)) (vec4 1/8)))

;;   (test-case "vec++"
;;     (define v
;;       (vec4 (vec4 0))) (check equal? v (vec4 0))
;;     (vec++ v) (check equal? v (vec4 1))
;;     (vec++ v) (check equal? v (vec4 2)))

;;   (test-case "vec--"
;;     (define v
;;       (vec4 (vec4 0))) (check equal? v (vec4 0))
;;     (vec-- v) (check equal? v (vec4 -1))
;;     (vec-- v) (check equal? v (vec4 -2)))

;;   (test-case "vec+"
;;     (check equal? (vec4 0) (vec+ (vec4 0)))
;;     (check equal? (vec4 1) (vec+ (vec4 1)))
;;     (check equal? (vec4 2) (vec+ (vec4 1) 1))
;;     (check equal? (vec4 2) (vec+ (vec4 1) (vec4 1)))
;;     (check equal? (vec4 3) (vec+ (vec4 1) 1 (vec4 1)))
;;     (check equal? (vec4 3) (vec+ (vec4 1) (vec4 2))))

;;   (test-case "vec-"
;;     (check equal? (vec4  0) (vec- (vec4 0)))
;;     (check equal? (vec4 -1) (vec- (vec4 1)))
;;     (check equal? (vec4  0) (vec- (vec4 1) 1))
;;     (check equal? (vec4  0) (vec- (vec4 1) (vec4 1)))
;;     (check equal? (vec4 -1) (vec- (vec4 1) 1 (vec4 1)))
;;     (check equal? (vec4 -1) (vec- (vec4 1) (vec4 2))))

;;   (test-case "vec*"
;;     (check equal? (vec4 0) (vec* (vec4)))
;;     (check equal? (vec4 0) (vec* (vec4 0) 0))
;;     (check equal? (vec4 0) (vec* (vec4) (vec4 0)))
;;     (check equal? (vec4 4) (vec* (vec4 2) 2))
;;     (check equal? (vec4 6) (vec* (vec4 2) (vec4 3)))
;;     (check equal? (vec4 6) (vec* (vec4 1) 2 (vec4 3))))

;;   (test-case "vec/"
;;     (check equal? (vec4 1) (vec/ (vec4 1)))
;;     (check equal? (vec4 2) (vec/ (vec4 6) 3))
;;     (check equal? (vec4 2) (vec/ (vec4 6) (vec4 3)))
;;     (check equal? (vec4 1) (vec/ (vec4 6) 3 (vec4 2)))))
