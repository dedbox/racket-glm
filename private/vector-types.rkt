#lang racket/base

(require template
         (for-syntax racket/base))

(provide (all-defined-out))

(struct tvec () #:transparent)
(struct tvec1 tvec (x) #:mutable #:transparent)
(struct tvec2 tvec (x y) #:mutable #:transparent)
(struct tvec3 tvec (x y z) #:mutable #:transparent)
(struct tvec4 tvec (x y z w) #:mutable #:transparent)

(for/template ([N (in-list '(1 2 3 4))])
  (for/template ([X (in-list '(x y z w))]
                 [R (in-list '(r g b a))]
                 [S (in-list '(s t p q))]
                 [_ (in-range N)])
    (define tvecN-R tvecN-X)
    (define tvecN-S tvecN-X)
    (define set-tvecN-R! set-tvecN-X!)
    (define set-tvecN-S! set-tvecN-X!)))
