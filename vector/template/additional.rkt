#lang template ($ N)

(require glm/private/vector-types
         glm/scalar
         racket/contract
         template
         (for-syntax racket/base))

(define/contract ($vecN-ref v i)
  (-> $vecN? (or/c (for/template ([K N]) K)) $scalar?)
  (case i
    (for/template ([X (in-list '(x y z w))]
                   [K (in-range N)])
      [(K) (tvecN-X v)])))

(define/contract ($vecN-set! v i x)
  (-> $vecN? (or/c (for/template ([K N]) K)) $scalar? void?)
  (case i
    (for/template ([X (in-list '(x y z w))]
                   [K (in-range N)])
      [(K) (set-tvecN-X! v x)])))

(define/contract ($vecN->list v) (-> $vecN? (listof $scalar?))
  (list (for/template ([X (in-list '(x y z w))]
                       [_ (in-range N)])
          (tvecN-X v))))

(define/contract ($vecN-map f . vs) (-> procedure? $vecN? (... ...) $vecN?)
  (apply $vecN (apply map (compose $scalar f) (map $vecN->list vs))))

(define/contract (in-$vecN v) (-> $vecN? sequence?)
  (in-list ($vecN->list v)))

(define-syntax-rule (for/$vecN clauses body (... ...))
  (apply $vecN (for/list clauses body (... ...))))

(define-syntax-rule (for*/$vecN clauses body (... ...))
  (apply $vecN (for*/list clauses body (... ...))))
