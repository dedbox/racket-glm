#lang racket/base

(require glm/factory
         glm/mat
         glm/vec)

(provide (all-defined-out))

;;; ----------------------------------------------------------------------------

(define (mat*= m a)
  (mat=! m (mat* m a)))

(define mat*
  (case-lambda
    [(m) m]
    [(a b) ((cond [(and (mat? a) (mat? b)) mat*mat]
                  [(mat? a) (if (vec? b) mat*vec mat*scalar)]
                  [(mat? b) (if (vec? a) vec*mat scalar*mat)]
                  [else (* a b)])
            a b)]
    [(a b . cs) (apply mat* (mat* a b) cs)]))

(define (mat*mat m1 m2)
  (unless (= (mat-num-cols m1) (mat-num-rows m2))
    (error 'mat* "expected a matrix with ~a rows" (mat-num-cols m1)))
  (apply (get-mat-constructor (mat-num-rows m1) (mat-num-cols m2))
         (for/list ([vk (in-mat-rows m2)])
           (mat*vec m1 vk))))

(define (mat*vec m v)
  (unless ((mat-col-predicate m) v)
    (error 'mat* "expected vec~a" (mat-num-rows m)))
  (apply (mat-row-constructor m)
         (for/list ([mk (in-mat-columns m)])
           (apply + (vec->list (vec* mk v))))))

(define (vec*mat v m)
  (unless ((mat-row-predicate m) v)
    (error 'mat* "expected vec~a" (mat-num-cols m)))
  (apply (mat-col-constructor m)
         (for/list ([mk (in-mat-rows m)])
           (apply + (vec->list (vec* mk v))))))

(define (mat*scalar m x)
  (apply (mat-constructor m) (for/list ([v (in-mat-columns m)]) (vec* v x))))

(define (scalar*mat x m)
  (mat*scalar m x))

;;; ----------------------------------------------------------------------------

(module+ test
  (require glm/mat4
           glm/vec4
           racket/function
           rackunit)

  ;; (define m (mat4 0.0 0.1 0.2 0.3
  ;;                 1.0 1.1 1.2 1.3
  ;;                 2.0 2.1 2.2 2.3
  ;;                 3.0 3.1 3.2 3.3))

  ;; (define m^t (mat4 0.0 1.0 2.0 3.0
  ;;                   0.1 1.1 2.1 3.1
  ;;                   0.2 1.2 2.2 3.2
  ;;                   0.3 1.3 2.3 3.3))

  ;; (define v (vec4 4.0 5.0 6.0 7.0))

  ;; (mat* m^t v)
  ;; (mat* v m^t)

  (test-case "mat*="
    (define m (mat4 one4x4))
    (check equal? m one4x4) (mat*= m one4x4)
    (check equal? m four4x4) (mat*= m one4x4)
    (check equal? m (mat4 (vec4 16.0))) (mat*= m one4x4)
    (check equal? m (mat4 (vec4 64.0))))

  (test-case "mat*"
    (check equal? zero4x4 (mat* zero4x4))
    (check equal? zero4x4 (mat* zero4x4 0.0))
    (check equal? zero4 (mat* zero4x4 zero4))
    (check equal? zero4x4 (mat* zero4x4 zero4x4))
    (check equal? four4x4 (mat* two4x4 2.0))
    (check equal? (mat* two4x4 three4) (vec4 24.0))
    (check equal? (mat* two4x4 three4x4) (mat4 (vec4 24.0)))
    (check equal? (mat* one4x4 2.0 three4) (vec4 24.0))
    (define m (mat4  1.0  2.0  3.0  4.0
                     5.0  6.0  7.0  8.0
                     9.0 10.0 11.0 12.0
                     13.0 14.0 15.0 16.0))
    (check equal? (mat* m (vec4 1.0 0.0 0.0 0.0)) (vec4 1.0 5.0 9.0 13.0))
    (check equal? (mat* m (vec4 0.0 1.0 0.0 0.0)) (vec4 2.0 6.0 10.0 14.0))
    (check equal? (mat* m (vec4 0.0 0.0 1.0 0.0)) (vec4 3.0 7.0 11.0 15.0))
    (check equal? (mat* m (vec4 0.0 0.0 0.0 1.0)) (vec4 4.0 8.0 12.0 16.0))
    (check equal? (mat* (vec4 1.0 0.0 0.0 0.0) m) (vec4  1.0  2.0  3.0  4.0))
    (check equal? (mat* (vec4 0.0 1.0 0.0 0.0) m) (vec4  5.0  6.0  7.0  8.0))
    (check equal? (mat* (vec4 0.0 0.0 1.0 0.0) m) (vec4  9.0 10.0 11.0 12.0))
    (check equal? (mat* (vec4 0.0 0.0 0.0 1.0) m) (vec4 13.0 14.0 15.0 16.0))
    ;; (set! m (mat4 0.0 0.1 0.2 0.3
    ;;               1.0 1.1 1.2 1.3
    ;;               2.0 2.1 2.2 2.3
    ;;               3.0 3.1 3.2 3.3))
    ;; (mat* m m)
    ))
