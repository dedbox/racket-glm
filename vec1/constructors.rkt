#lang template ($)

(require glm/private/types
         racket/contract
         (for-syntax racket/base))

(struct $vec1 tvec1 ()
  #:transparent
  #:name glm-$vec1
  #:constructor-name make-$vec1)

(define/contract $vec1 (case-> (-> $vec1?) (-> any/c $vec1?))
  (case-lambda
    [() (make-$vec1 ($scalar 0))]
    [(a) (make-$vec1 (cond [($scalar? a) a]
                           [($vec1? a) (tvec1-x a)]
                           [(tvec1? a) ($scalar (tvec1-x a))]
                           [(tvec2? a) ($scalar (tvec2-x a))]
                           [(tvec3? a) ($scalar (tvec3-x a))]
                           [(tvec4? a) ($scalar (tvec4-x a))]
                           [else ($scalar a)]))]))
