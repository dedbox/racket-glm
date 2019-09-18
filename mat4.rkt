#lang racket/base

;;; ----------------------------------------------------------------------------
;;; 4x4 Matrices
;;;
;;; glm/detail/type_mat4x3.hpp
;;; glm/detail/type_mat4x4.inl

(require glm/mat
         glm/vec
         glm/vec4)

(provide (all-defined-out))

(struct mat4 glm:mat ()
  #:name glm:mat4
  #:constructor-name make-mat4
  #:methods gen:glm-mat
  [(define (mat-name _) 'mat4)
   (define (mat-num-cols _) 4)
   (define (mat-num-rows _) 4)
   (define (mat-predicate _) mat4?)
   (define (mat-constructor _) mat4)
   (define (mat-col-predicate _) vec4?)
   (define (mat-col-constructor _) vec4)
   (define (mat-row-predicate _) vec4?)
   (define (mat-row-constructor _) vec4)
   (define (mat-make-col-vec _) make-vec4)])

(define mat4
  (case-lambda
    [() (mat4 1.0)]
    [(a) (cond [(mat4? a) (mat-copy a)]
               [(vec4? a) (mat4 a a a a)]
               [else (mat4  a  0.0 0.0 0.0
                           0.0  a  0.0 0.0
                           0.0 0.0  a  0.0
                           0.0 0.0 0.0  a )])]
    [(x y z w)
     (define-values (x0 x1 x2 x3
                     y0 y1 y2 y3
                     z0 z1 z2 z3
                     w0 w1 w2 w3)
       (apply values (apply append (map vec->list (list x y z w)))))
     (mat4 x0 y0 z0 w0
           x1 y1 z1 w1
           x2 y2 z2 w2
           x3 y3 z3 w3)]
    [(x0 y0 z0 w0
      x1 y1 z1 w1
      x2 y2 z2 w2
      x3 y3 z3 w3)
     (make-mat4 (make-mat-data 4 4
                               x0 y0 z0 w0
                               x1 y1 z1 w1
                               x2 y2 z2 w2
                               x3 y3 z3 w3))]))

(define id4x4 (mat4 1.0))

(define-values (zero4x4 one4x4 two4x4 three4x4 four4x4
                        five4x4 six4x4 seven4x4 eight4x4 nine4x4)
  (apply values (for/list ([k (in-range 10)])
                  (mat4 (vec4 (real->single-flonum k))))))

(define-values (-one4x4 -two4x4 -three4x4
                        -four4x4 -five4x4 -six4x4 -seven4x4 -eight4x4 -nine4x4)
  (apply values (for/list ([k (in-range 1 10)])
                  (mat4 (vec4 (- (real->single-flonum k)))))))

;;; ----------------------------------------------------------------------------

(module+ test
  (require racket/function
           rackunit)

  (define m (mat4 0.0 0.1 0.2 0.3
                  1.0 1.1 1.2 1.3
                  2.0 2.1 2.2 2.3
                  3.0 3.1 3.2 3.3))

  (test-case "column-major order"
    (check equal? (mat-columns m)
           (list (vec4 0.0 0.1 0.2 0.3)
                 (vec4 1.0 1.1 1.2 1.3)
                 (vec4 2.0 2.1 2.2 2.3)
                 (vec4 3.0 3.1 3.2 3.3))))

  (test-case "mat4 and mat->list are isomorphic"
    (check equal? (apply mat4 (mat->list m)) m))

  (test-case "equal?"
    (check equal? zero4x4 zero4x4)
    (check equal? one4x4 one4x4)
    (check equal? id4x4 id4x4)
    (check (negate equal?) one4x4 zero4x4)
    (check (negate equal?) zero4x4 one4x4)
    (check (negate equal?) one4x4 id4x4))

  (test-case "mac+="
    (define m (mat4 zero4x4))
    (check equal? m zero4x4) (mat+= m one4x4)
    (check equal? m one4x4) (mat+= m one4x4)
    (check equal? m two4x4))

  (test-case "mat-="
    (define m (mat4 one4x4))
    (check equal? m one4x4) (mat-= m one4x4)
    (check equal? m zero4x4) (mat-= m one4x4)
    (check equal? m -one4x4))

  ;; (test-case "mat/="
  ;;   (define m (mat4 one4x4))
  ;;   (check equal? m (mat4 (vec4 1.0))) (mat/= m two4x4)
  ;;   (check equal? m (mat4 (vec4 0.5))) (mat/= m five4x4)
  ;;   (check equal? m (mat4 (vec4 0.1))))

  (test-case "mat+"
    (check equal? zero4x4 (mat+ zero4x4))
    (check equal? one4x4 (mat+ one4x4))
    (check equal? two4x4 (mat+ one4x4 1.0))
    (check equal? two4x4 (mat+ one4x4 one4x4))
    (check equal? three4x4 (mat+ one4x4 1.0 one4x4))
    (check equal? three4x4 (mat+ one4x4 two4x4)))

  (test-case "mat-"
    (check equal? zero4x4 (mat- zero4x4))
    (check equal? -one4x4 (mat- one4x4))
    (check equal? zero4x4 (mat- one4x4 1.0))
    (check equal? zero4x4 (mat- one4x4 one4))
    (check equal? -one4x4 (mat- one4x4 1.0 one4))
    (check equal? -one4x4 (mat- one4x4 two4)))

  ;; (test-case "mat/"
  ;;   (check equal? one4x4 (mat/ one4x4))
  ;;   (check equal? two4x4 (mat/ six4x4 3.0))
  ;;   (check equal? two4x4 (mat/ six4x4 three4))
  ;;   (check equal? one4x4 (mat/ six4x4 3.0 two4)))

  (test-case "mat++"
    (define m (mat4 zero4x4))
    (check equal? m zero4x4) (mat++ m)
    (check equal? m one4x4) (mat++ m)
    (check equal? m two4x4))

  (test-case "mat--"
    (define m (mat4 zero4x4))
    (check equal? m zero4x4) (mat-- m)
    (check equal? m -one4x4) (mat-- m)
    (check equal? m -two4x4)))
