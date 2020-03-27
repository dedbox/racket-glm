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
  (define/contract set-$vecN-X! (-> $vecN? $scalar? void?) set-tvecN-X!)
  (define/contract set-$vecN-R! (-> $vecN? $scalar? void?) set-tvecN-R!)
  (define/contract set-$vecN-S! (-> $vecN? $scalar? void?) set-tvecN-S!))
