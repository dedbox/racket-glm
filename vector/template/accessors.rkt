#lang template ($ N)

(require glm/private/vector-types
         glm/scalar
         racket/contract
         template
         (for-syntax racket/base))

(for/template ([X (in-list '(x y z w))]
               [R (in-list '(r g b a))]
               [S (in-list '(s t p q))]
               [_ (in-range N)])
  (define/contract $vecN-X (-> $vecN? $scalar?) tvecN-X)
  (define/contract $vecN-R (-> $vecN? $scalar?) tvecN-R)
  (define/contract $vecN-S (-> $vecN? $scalar?) tvecN-S))
