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

(define/contract ($vecN->list v) (-> $vecN? (listof $scalar?))
  (list (for/template ([X (in-list '(x y z w))]
                       [_ (in-range N)])
          (tvecN-X v))))
