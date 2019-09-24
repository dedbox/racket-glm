#lang racket/base

(require glm/mat
         glm/matrix
         glm/vec)

(provide (all-defined-out))

(define (mat/= m . args)
  (mat=! m (apply mat/ m args)))

(define mat/
  (let ()

    (define (mat/mat m1 m2)
      (define m1-copy (mat-copy m1))
      (mat* m1 (inverse m2)))

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

    (case-lambda
      [(a) (cond [(mat? a) (inverse a)]
                 [(vec? a) (vec/ a)]
                 [else (/ a)])]
      [(a b) ((cond [(and (mat? a) (mat? b)) mat/mat]
                    [(mat? a) (if (vec? b) mat/vec mat/scalar)]
                    [(mat? b) (if (vec? a) vec/mat scalar/mat)]
                    [else vec/])
              a b)]
      [(a b . cs) (apply mat/ (mat/ a b) cs)])))

;;; ----------------------------------------------------------------------------

(module+ test
  (require rackunit)

  ;; (test-case "mat/="
  ;;   (define m (mat4 1)) (check equal? m (mat4  1 ))
  ;;   (mat/=  m (mat4 2)) (check equal? m (mat4 1/2))
  ;;   (mat/=  m (mat4 5))
  ;;   (check equal? m (mat4 0.1)))

  )
