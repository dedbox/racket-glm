#lang racket/base

(require ffi/unsafe
         ffi/vector
         glm/private/matrix
         glm/vec)

(provide (all-defined-out))

(define-matrix-type mat
  #:vector-type vec
  #:c-type     _float
  #:ffi-vector f32vector)

;;; ----------------------------------------------------------------------------

;; (module+ test
;;   (require racket/function
;;            rackunit)

;;   (define m (mat3 0.0 0.1 0.2
;;                   1.0 1.1 1.2
;;                   2.0 2.1 2.2))

;;   (test-case "column-major order (3)"
;;     (check equal? (mat-columns m)
;;            (list (vec3 0.0 0.1 0.2)
;;                  (vec3 1.0 1.1 1.2)
;;                  (vec3 2.0 2.1 2.2))))

;;   (test-case "mat3 and mat->list are isomorphic"
;;     (check equal? (apply mat3 (mat->list m)) m))

;;   (test-case "equal?"
;;     (check equal? (mat3 0) (mat3 0))
;;     (check equal? (mat3 1) (mat3 1))
;;     (check equal? (mat3) (mat3))
;;     (check (negate equal?) (mat3 1) (mat3 0))
;;     (check (negate equal?) (mat3 0) (mat3 1))
;;     (check (negate equal?) (mat3 (vec3 1)) (mat3)))

;;   (test-case "mat+="
;;     (define m
;;       (mat3 (mat3 0))) (check equal? m (mat3 0))
;;     (mat+= m (mat3 1)) (check equal? m (mat3 1))
;;     (mat+= m (mat3 1)) (check equal? m (mat3 2)))

;;   (test-case "mat-="
;;     (define m
;;       (mat3 (mat3 1))) (check equal? m (mat3  1))
;;     (mat-= m (mat3 1)) (check equal? m (mat3  0))
;;     (mat-= m (mat3 1)) (check equal? m (mat3 -1)))

;;   (test-case "mat+"
;;     (check equal? (mat+ (mat3 0)) (mat3 0))
;;     (check equal? (mat+ (mat3 1)) (mat3 1))
;;     (check equal? (mat+ (mat3 (vec3 1)) 1) (mat3 (vec3 2)))
;;     (check equal? (mat+ (mat3 (vec3 1)) (mat3 (vec3 1))) (mat3 (vec3 2)))
;;     (check equal? (mat+ (mat3 (vec3 1)) 1 (mat3 (vec3 1))) (mat3 (vec3 3)))
;;     (check equal? (mat+ (mat3 (vec3 1)) (mat3 (vec3 2))) (mat3 (vec3 3))))

;;   (test-case "mat-"
;;     (check equal? (mat- (mat3 0)) (mat3  0))
;;     (check equal? (mat- (mat3 1)) (mat3 -1))
;;     (check equal? (mat- (mat3 (vec3 1)) 1) (mat3 (vec3  0)))
;;     (check equal? (mat- (mat3 (vec3 1)) (vec3 1)) (mat3 (vec3  0)))
;;     (check equal? (mat- (mat3 (vec3 1)) 1 (vec3 1)) (mat3 (vec3 -1)))
;;     (check equal? (mat- (mat3 (vec3 1)) (vec3 2)) (mat3 (vec3 -1))))

;;   (test-case "mat++"
;;     (define m (mat3 (mat3 (vec3 0)))) (check equal? m (mat3 (vec3 0)))
;;     (mat++ m) (check equal? m (mat3 (vec3 1)))
;;     (mat++ m) (check equal? m (mat3 (vec3 2))))

;;   (test-case "mat--"
;;     (define m (mat3 (mat3 (vec3 0)))) (check equal? m (mat3 (vec3 0)))
;;     (mat-- m) (check equal? m (mat3 (vec3 -1)))
;;     (mat-- m) (check equal? m (mat3 (vec3 -2))))

;;   ;; ...........................................................................

;;   (set! m (mat4 0.0 0.1 0.2 0.3
;;                 1.0 1.1 1.2 1.3
;;                 2.0 2.1 2.2 2.3
;;                 3.0 3.1 3.2 3.3))

;;   (test-case "column-major order (4)"
;;     (check equal? (mat-columns m)
;;            (list (vec4 0.0 0.1 0.2 0.3)
;;                  (vec4 1.0 1.1 1.2 1.3)
;;                  (vec4 2.0 2.1 2.2 2.3)
;;                  (vec4 3.0 3.1 3.2 3.3))))

;;   (test-case "equal?"
;;     (check equal? (mat4 0) (mat4 0))
;;     (check equal? (mat4 1) (mat4 1))
;;     (check equal? (mat4) (mat4))
;;     (check (negate equal?) (mat4 1) (mat4 0))
;;     (check (negate equal?) (mat4 0) (mat4 1))
;;     (check (negate equal?) (mat4 0) (mat4)))

;;   (test-case "mac+="
;;     (define m
;;       (mat4 (mat4 0))) (check equal? m (mat4 0))
;;     (mat+= m (mat4 1)) (check equal? m (mat4 1))
;;     (mat+= m (mat4 1)) (check equal? m (mat4 2)))

;;   (test-case "mat-="
;;     (define m
;;       (mat4 (mat4 1))) (check equal? m (mat4 1))
;;     (mat-= m (mat4 1)) (check equal? m (mat4 0))
;;     (mat-= m (mat4 1)) (check equal? m (mat4 -1)))

;;   (test-case "mat+"
;;     (check equal? (mat4 (vec4 0)) (mat+ (mat4 (vec4 0))))
;;     (check equal? (mat4 (vec4 1)) (mat+ (mat4 (vec4 1))))
;;     (check equal? (mat4 (vec4 2)) (mat+ (mat4 (vec4 1)) 1))
;;     (check equal? (mat4 (vec4 2)) (mat+ (mat4 (vec4 1)) (mat4 (vec4 1))))
;;     (check equal? (mat4 (vec4 3)) (mat+ (mat4 (vec4 1)) 1 (mat4 (vec4 1))))
;;     (check equal? (mat4 (vec4 3)) (mat+ (mat4 (vec4 1)) (mat4 (vec4 2)))))

;;   (test-case "mat-"
;;     (check equal? (mat4 (vec4 0)) (mat- (mat4 (vec4 0))))
;;     (check equal? (mat4 (vec4 -1)) (mat- (mat4 (vec4 1))))
;;     (check equal? (mat4 (vec4 0)) (mat- (mat4 (vec4 1)) 1))
;;     (check equal? (mat4 (vec4 0)) (mat- (mat4 (vec4 1)) (vec4 1)))
;;     (check equal? (mat4 (vec4 -1)) (mat- (mat4 (vec4 1)) 1 (vec4 1)))
;;     (check equal? (mat4 (vec4 -1)) (mat- (mat4 (vec4 1)) (vec4 2))))

;;   ;; (test-case "mat/"
;;   ;;   (check equal? (mat4 1) (mat/ (mat4 1)))
;;   ;;   (check equal? (mat4 2) (mat/ (mat4 6) 3.0))
;;   ;;   (check equal? (mat4 2) (mat/ (mat4 6) (vec4 3)))
;;   ;;   (check equal? (mat4 1) (mat/ (mat4 6) 3.0 (vec4 2))))

;;   (test-case "mat++"
;;     (define m (mat4 (mat4 0)))
;;     (check equal? m (mat4 0))
;;     (mat++ m) (check equal? m (mat4 (vec4 1)))
;;     (mat++ m) (check equal? m (mat4 (vec4 2))))

;;   (test-case "mat--"
;;     (define m (mat4 (mat4 0)))
;;     (check equal? m (mat4 0))
;;     (mat-- m) (check equal? m (mat4 (vec4 -1)))
;;     (mat-- m) (check equal? m (mat4 (vec4 -2))))

;;   ;; ...........................................................................

;;   (test-case "mat*="
;;     (define m
;;       (mat4 (vec4 1))) (check equal? m (mat4 (vec4 1)))
;;     (mat*= m (mat4 (vec4 1))) (check equal? m (mat4 (vec4 4)))
;;     (mat*= m (mat4 (vec4 1))) (check equal? m (mat4 (vec4 16)))
;;     (mat*= m (mat4 (vec4 1))) (check equal? m (mat4 (vec4 64))))

;;   (test-case "mat*"
;;     (check equal? (mat* (mat4 (vec4 0))) (mat4 (vec4 0)))
;;     (check equal? (mat* (mat4 (vec4 0)) 0) (mat4 (vec4 0)))
;;     (check equal? (mat* (mat4 (vec4 0)) (vec4 0)) (vec4 0))
;;     (check equal? (mat* (mat4 (vec4 0)) (mat4 (vec4 0))) (mat4 (vec4 0)))
;;     (check equal? (mat* (mat4 (vec4 2)) 2) (mat4 (vec4 4)))
;;     (check equal? (mat* (mat4 (vec4 2)) (vec4 3)) (vec4 24))
;;     (check equal? (mat* (mat4 (vec4 2)) (mat4 3) (vec4 4)) (vec4 96))
;;     (check equal? (mat* (vec4 (vec4 1)) 2 (vec4 3)) (vec4 6))
;;     (define m (mat4 1  2  3  4
;;                     5  6  7  8
;;                     9 10 11 12
;;                    13 14 15 16))
;;     (check equal? (mat* m (vec4 1 0 0 0)) (vec4 1 2 3 4))
;;     (check equal? (mat* m (vec4 0 1 0 0)) (vec4 5 6 7 8))
;;     (check equal? (mat* m (vec4 0 0 1 0)) (vec4 9 10 11 12))
;;     (check equal? (mat* m (vec4 0 0 0 1)) (vec4 13 14 15 16))
;;     (check equal? (mat* (vec4 1 0 0 0) m) (vec4 1 5  9 13))
;;     (check equal? (mat* (vec4 0 1 0 0) m) (vec4 2 6 10 14))
;;     (check equal? (mat* (vec4 0 0 1 0) m) (vec4 3 7 11 15))
;;     (check equal? (mat* (vec4 0 0 0 1) m) (vec4 4 8 12 16))))
