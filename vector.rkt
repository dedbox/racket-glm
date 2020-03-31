#lang racket/base

(require glm/private/reprovide
         glm/private/vector-types
         template
         (for-syntax racket/base))

(reprovide glm/vector/boolean
           glm/vector/double
           glm/vector/int
           glm/vector/uint)

(begin-template
  (provide
   (for/template ([N (in-list '(1 2 3 4))])
     tvecN?
     (for/template ([X (in-list '(x y z w))]
                    [R (in-list '(r g b a))]
                    [S (in-list '(s t p q))]
                    [_ (in-range N)])
       tvecN-X set-tvecN-X!
       tvecN-R set-tvecN-R!
       tvecN-S set-tvecN-S!))))
