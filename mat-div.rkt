#lang racket/base

(require glm/mat
         glm/mat-mul
         glm/matrix
         glm/vec)

(provide (all-defined-out))

;;; ----------------------------------------------------------------------------

(define (mat/= m . args)
  (mat=! m (apply mat/ m args)))

(define mat/
  (case-lambda
    [(m) (inverse m)]
    [(a b) ((cond [(and (mat? a) (mat? b)) mat/mat]
                  [(mat? a) (if (vec? b) mat/vec mat/scalar)]
                  [(mat? b) (if (vec? a) vec/mat scalar/mat)]
                  [else (* a b)])
            a b)]))

(define (mat/mat m1 m2)
  (define m1-copy (mat-copy m1))
  (mat*= m1-copy (inverse m1-copy))
  m1-copy)

(define (mat/vec m v)
  (unless (= (mat-num-cols m) (vec-length v))
    (error 'mat/ "expected vec~a" (mat-num-cols m)))
  (mat* (inverse m) v))

(define (vec/mat v m)
  (unless (= (mat-num-rows m) (vec-length v))
    (error 'mat/ "expected vec~a" (mat-num-rows m)))
  (mat* v (inverse m)))

(define (mat/scalar m x)
  (apply (mat-constructor m) (for/list ([v (in-mat-columns m)]) (vec/ v x))))

(define (scalar/mat x m)
  (apply (mat-constructor m) (for/list ([v (in-mat-columns m)]) (vec/ x v))))
