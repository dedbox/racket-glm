#lang template ($ N)

(require glm/private/vector-types
         glm/scalar
         racket/contract
         template
         (for-syntax racket/base))

(define/contract ($vecN->list v) (-> $vecN? (listof $scalar?))
  (begin-template (list (for/template ([X (in-list '(x y z w))]
                                       [_ (in-range N)])
                          (tvecN-X v)))))
