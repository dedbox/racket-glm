#lang racket/base

(require ffi/unsafe
         ffi/vector
         glm/private/vector)

(provide (except-out (all-defined-out) current-ivec-precision))

(define-numeric-vector-type ivec _int integer? exact-integer?
  #:from-scalar values
  #:to-scalar (λ (a) (if (inexact? a) (inexact->exact a) a))
  #:to-native values
  #:ffi s32vector)

;;; ============================================================================

;; (module+ test
;;   (require racket/function
;;            rackunit)

;;   (test-case "equal?"
;;     (check equal? (ivec2 0) (ivec2 0))
;;     (check equal? (ivec2 1) (ivec2 1))
;;     (check (negate equal?) (ivec2 1) (ivec2 0))
;;     (check (negate equal?) (ivec2 0) (ivec2 1)))

;;   (test-case "ivec+="
;;     (define v
;;       (ivec2 (ivec2 0))) (check equal? v (ivec2 0))
;;     (ivec+= v (ivec2 1)) (check equal? v (ivec2 1))
;;     (ivec+= v (ivec2 1)) (check equal? v (ivec2 2)))

;;   (test-case "ivec-="
;;     (define v
;;       (ivec2 (ivec2 1))) (check equal? v (ivec2  1))
;;     (ivec-= v (ivec2 1)) (check equal? v (ivec2  0))
;;     (ivec-= v (ivec2 1)) (check equal? v (ivec2 -1)))

;;   (test-case "ivec*="
;;     (define v
;;       (ivec2 (ivec2 1))) (check equal? v (ivec2 1))
;;     (ivec*= v (ivec2 1)) (check equal? v (ivec2 1))
;;     (ivec*= v (ivec2 2)) (check equal? v (ivec2 2))
;;     (ivec*= v (ivec2 2)) (check equal? v (ivec2 4)))

;;   (test-case "ivec/="
;;     (define v
;;       (ivec2 (ivec2 6))) (check equal? v (ivec2 6))
;;     (ivec/= v (ivec2 3)) (check equal? v (ivec2 2))
;;     (ivec/= v (ivec2 2)) (check equal? v (ivec2 1))
;;     (check-exn exn:fail:contract? (λ () (ivec/= v (ivec2 2)))))

;;   (test-case "ivec+"
;;     (check equal? (ivec+ (ivec2 1)) (ivec2 1))
;;     (check equal? (ivec+ (ivec2 1) (ivec2 1)) (ivec2 2))
;;     (check equal? (ivec+ (ivec2 1) (ivec2 1) (ivec2 1)) (ivec2 3)))

;;   (test-case "ivec-"
;;     (check equal? (ivec- (ivec2 0)) (ivec2 0))
;;     (check equal? (ivec- (ivec2 1) (ivec2 0)) (ivec2  1))
;;     (check equal? (ivec- (ivec2 1) (ivec2 1)) (ivec2  0))
;;     (check equal? (ivec- (ivec2 1) (ivec2 1) (ivec2 1)) (ivec2 -1)))

;;   (test-case "ivec*"
;;     (check equal? (ivec* (ivec2 0)) (ivec2 0))
;;     (check equal? (ivec* (ivec2 1)) (ivec2 1))
;;     (check equal? (ivec* (ivec2 2)) (ivec2 2))
;;     (check equal? (ivec* (ivec2 2) (ivec2 3)) (ivec2 6))
;;     (check equal? (ivec* (ivec2 2) (ivec2 3) (ivec2 4)) (ivec2 24))
;;     (check equal? (ivec* (ivec2 1 2) (ivec2 2 3) (ivec2 3 4)) (ivec2 6 24))
;;     (check equal? (ivec* (ivec2 1) -1) (ivec2 -1)))

;;   (test-case "ivec/"
;;     (check equal? (ivec/ (ivec2 1)) (ivec2 1))
;;     (check equal? (ivec/ (ivec2 0) (ivec2 1)) (ivec2 0))
;;     (check equal? (ivec/ (ivec2 2) (ivec2 1)) (ivec2 2))
;;     (check equal? (ivec/ (ivec2 6) (ivec2 2) (ivec2 1)) (ivec2 3))
;;     (check-exn exn:fail:contract? (λ () (ivec/ (ivec2 1) (ivec2 2)))))

;;   ;;; ..........................................................................

;;   (test-case "equal?"
;;     (check equal? (ivec3 0) (ivec3 0))
;;     (check equal? (ivec3 1) (ivec3 1))
;;     (check (negate equal?) (ivec3 1) (ivec3 0))
;;     (check (negate equal?) (ivec3 0) (ivec3 1)))

;;   (test-case "ivec+="
;;     (define v
;;       (ivec3 (ivec3 0))) (check equal? v (ivec3 0))
;;     (ivec+= v (ivec3 1)) (check equal? v (ivec3 1))
;;     (ivec+= v (ivec3 1)) (check equal? v (ivec3 2)))

;;   (test-case "ivec-="
;;     (define v
;;       (ivec3 (ivec3 1))) (check equal? v (ivec3  1))
;;     (ivec-= v (ivec3 1)) (check equal? v (ivec3  0))
;;     (ivec-= v (ivec3 1)) (check equal? v (ivec3 -1)))

;;   (test-case "ivec*="
;;     (define v
;;       (ivec3 (ivec3 1))) (check equal? v (ivec3 1))
;;     (ivec*= v (ivec3 1)) (check equal? v (ivec3 1))
;;     (ivec*= v (ivec3 2)) (check equal? v (ivec3 2))
;;     (ivec*= v (ivec3 2)) (check equal? v (ivec3 4)))

;;   (test-case "ivec/="
;;     (define v
;;       (ivec3 (ivec3 6))) (check equal? v (ivec3 6))
;;     (ivec/= v (ivec3 3)) (check equal? v (ivec3 2))
;;     (ivec/= v (ivec3 2)) (check equal? v (ivec3 1))
;;     (check-exn exn:fail:contract? (λ () (ivec/= v (ivec3 2)))))

;;   (test-case "ivec+"
;;     (check equal? (ivec+ (ivec3 1)) (ivec3 1))
;;     (check equal? (ivec+ (ivec3 1) (ivec3 1)) (ivec3 2))
;;     (check equal? (ivec+ (ivec3 1) (ivec3 1) (ivec3 1)) (ivec3 3)))

;;   (test-case "ivec-"
;;     (check equal? (ivec- (ivec3 0)) (ivec3 0))
;;     (check equal? (ivec- (ivec3 1) (ivec3 0)) (ivec3  1))
;;     (check equal? (ivec- (ivec3 1) (ivec3 1)) (ivec3  0))
;;     (check equal? (ivec- (ivec3 1) (ivec3 1) (ivec3 1)) (ivec3 -1)))

;;   (test-case "ivec*"
;;     (check equal? (ivec* (ivec3 0)) (ivec3 0))
;;     (check equal? (ivec* (ivec3 1)) (ivec3 1))
;;     (check equal? (ivec* (ivec3 2)) (ivec3 2))
;;     (check equal? (ivec* (ivec3 2) (ivec3 3)) (ivec3 6))
;;     (check equal? (ivec* (ivec3 2) (ivec3 3) (ivec3 4)) (ivec3 24))
;;     (check equal? (ivec* (ivec3 1 2 3) (ivec3 2 3 4) (ivec3 3 4 5)) (ivec3 6 24 60))
;;     (check equal? (ivec* (ivec3 1) -1) (ivec3 -1)))

;;   (test-case "ivec/"
;;     (check equal? (ivec/ (ivec3 1)) (ivec3 1))
;;     (check equal? (ivec/ (ivec3 0) (ivec3 1)) (ivec3 0))
;;     (check equal? (ivec/ (ivec3 2) (ivec3 1)) (ivec3 2))
;;     (check equal? (ivec/ (ivec3 6) (ivec3 2) (ivec3 1)) (ivec3 3))
;;     (check-exn exn:fail:contract? (λ () (ivec/ (ivec3 1) (ivec3 2)))))

;;   ;;; ..........................................................................

;;   (test-case "equal?"
;;     (check equal? (ivec4 0) (ivec4 0))
;;     (check equal? (ivec4 1) (ivec4 1))
;;     (check (negate equal?) (ivec4 1) (ivec4 0))
;;     (check (negate equal?) (ivec4 0) (ivec4 1)))

;;   (test-case "ivec+="
;;     (define v
;;       (ivec4 (ivec4 0))) (check equal? v (ivec4 0))
;;     (ivec+= v (ivec4 1)) (check equal? v (ivec4 1))
;;     (ivec+= v (ivec4 1)) (check equal? v (ivec4 2)))

;;   (test-case "ivec-="
;;     (define v
;;       (ivec4 (ivec4 1))) (check equal? v (ivec4  1))
;;     (ivec-= v (ivec4 1)) (check equal? v (ivec4  0))
;;     (ivec-= v (ivec4 1)) (check equal? v (ivec4 -1)))

;;   (test-case "ivec*="
;;     (define v
;;       (ivec4 (ivec4 1))) (check equal? v (ivec4 1))
;;     (ivec*= v (ivec4 1)) (check equal? v (ivec4 1))
;;     (ivec*= v (ivec4 2)) (check equal? v (ivec4 2))
;;     (ivec*= v (ivec4 2)) (check equal? v (ivec4 4)))

;;   (test-case "ivec/="
;;     (define v
;;       (ivec4 (ivec4 6))) (check equal? v (ivec4 6))
;;     (ivec/= v (ivec4 3)) (check equal? v (ivec4 2))
;;     (ivec/= v (ivec4 2)) (check equal? v (ivec4 1))
;;     (check-exn exn:fail:contract? (λ () (ivec/= v (ivec4 2)))))

;;   (test-case "ivec+"
;;     (check equal? (ivec+ (ivec4 1)) (ivec4 1))
;;     (check equal? (ivec+ (ivec4 1) (ivec4 1)) (ivec4 2))
;;     (check equal? (ivec+ (ivec4 1) (ivec4 1) (ivec4 1)) (ivec4 3)))

;;   (test-case "ivec-"
;;     (check equal? (ivec- (ivec4 0)) (ivec4 0))
;;     (check equal? (ivec- (ivec4 1) (ivec4 0)) (ivec4 1))
;;     (check equal? (ivec- (ivec4 1) (ivec4 1)) (ivec4 0))
;;     (check equal? (ivec- (ivec4 1) (ivec4 1) (ivec4 1)) (ivec4 -1)))

;;   (test-case "ivec*"
;;     (check equal? (ivec* (ivec4 0)) (ivec4 0))
;;     (check equal? (ivec* (ivec4 1)) (ivec4 1))
;;     (check equal? (ivec* (ivec4 2)) (ivec4 2))
;;     (check equal? (ivec* (ivec4 2) (ivec4 3)) (ivec4 6))
;;     (check equal? (ivec* (ivec4 2) (ivec4 3) (ivec4 4)) (ivec4 24))
;;     (check equal? (ivec* (ivec4 1 2 3 4) (ivec4 2 3 4 5) (ivec4 3 4 5 6)) (ivec4 6 24 60 120))
;;     (check equal? (ivec* (ivec4 1) -1) (ivec4 -1)))

;;   (test-case "ivec/"
;;     (check equal? (ivec/ (ivec4 1)) (ivec4 1))
;;     (check equal? (ivec/ (ivec4 0) (ivec4 1)) (ivec4 0))
;;     (check equal? (ivec/ (ivec4 2) (ivec4 1)) (ivec4 2))
;;     (check equal? (ivec/ (ivec4 6) (ivec4 2) (ivec4 1)) (ivec4 3))
;;     (check-exn exn:fail:contract? (λ () (ivec/ (ivec4 1) (ivec4 2)))))

;;   (test-case "ivec++"
;;     (define v (ivec4 (ivec4 -1))) (check equal? v (ivec4 -1))
;;     (ivec++ v) (check equal? v (ivec4 0))
;;     (ivec++ v) (check equal? v (ivec4 1))
;;     (ivec++ v) (check equal? v (ivec4 2)))

;;   (test-case "ivec--"
;;     (define v (ivec4 (ivec4 2))) (check equal? v (ivec4 2))
;;     (ivec-- v) (check equal? v (ivec4  1))
;;     (ivec-- v) (check equal? v (ivec4  0))
;;     (ivec-- v) (check equal? v (ivec4 -1))))
