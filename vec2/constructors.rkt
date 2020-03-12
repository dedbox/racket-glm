#lang template ($)

(require glm/private/types
         racket/contract
         (for-syntax racket/base))

(struct $vec2 tvec2 ()
  #:transparent
  #:name glm-$vec2
  #:constructor-name make-$vec2)

(define/contract $vec2 (case-> (-> $vec2?)
                               (-> any/c $vec2?)
                               (-> any/c any/c $vec2?))
  (case-lambda
    [() (make-$vec2 ($scalar 0) ($scalar 0))]
    [(a)
     (apply make-$vec2
            (cond [($scalar? a) (list a a)]
                  [($vec2? a) (list (tvec2-x a) (tvec2-y a))]
                  [(tvec1? a) (list ($scalar (tvec1-x a)) ($scalar (tvec1-x a)))]
                  [(tvec2? a) (list ($scalar (tvec2-x a)) ($scalar (tvec2-y a)))]
                  [(tvec3? a) (list ($scalar (tvec3-x a)) ($scalar (tvec3-y a)))]
                  [(tvec4? a) (list ($scalar (tvec4-x a)) ($scalar (tvec4-y a)))]
                  [else (list ($scalar a) ($scalar a))]))]
    [(a b)
     (apply make-$vec2
            (cond [(and ($scalar? a) ($scalar? b)) (list a b)]
                  [(and (tvec1? a) (tvec1? b)) (list ($scalar (tvec1-x a))
                                                     ($scalar (tvec1-x b)))]
                  [(tvec1? a) (list ($scalar (tvec1-x a)) ($scalar b))]
                  [(tvec1? b) (list ($scalar a) ($scalar (tvec1-x b)))]
                  [else (list ($scalar a) ($scalar b))]))]))
