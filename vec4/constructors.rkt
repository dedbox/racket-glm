#lang template ($)

(require glm/private/types
         racket/contract)

(struct $vec4 tvec4 ()
  #:transparent
  #:name glm-$vec4
  #:constructor-name make-$vec4)

(define/contract $vec4 (case-> (-> $vec4?)
                               (-> any/c $vec4?)
                               (-> any/c any/c $vec4?)
                               (-> any/c any/c any/c $vec4?)
                               (-> any/c any/c any/c any/c $vec4?))
  (case-lambda
    [() (apply make-$vec4 (build-list 4 (λ _ ($scalar 0))))]
    [(a)
     (apply make-$vec4
            (cond [($scalar? a) (list a a a a)]
                  [($vec4? a) (list (tvec4-x a) (tvec4-y a) (tvec4-z a) (tvec4-w a))]
                  [(tvec1? a) (build-list 4 (λ _ ($scalar (tvec1-x a))))]
                  [(tvec4? a) (map $scalar (list (tvec4-x a) (tvec4-y a) (tvec4-z a)))]
                  [else (build-list 4 (λ _ ($scalar a)))]))]
    [(a b)
     (apply make-$vec4
            (cond [(and (tvec3? a) (tvec1? b))
                   (map $scalar (list (tvec3-x a) (tvec3-y a) (tvec3-z a) (tvec1-x b)))]
                  [(and (tvec1? a) (tvec3? b))
                   (map $scalar (list (tvec1-x a) (tvec3-x b) (tvec3-x b) (tvec3-z b)))]
                  [(and (tvec2? a) (tvec2? b))
                   (map $scalar (list (tvec2-x a) (tvec2-y a) (tvec2-x b) (tvec2-y b)))]
                  [(tvec3? a) (map $scalar (list (tvec3-x a) (tvec3-y a) (tvec3-z a) b))]
                  [(tvec3? b) (map $scalar (list a (tvec3-x b) (tvec3-y b) (tvec3-z b)))]
                  [else (raise-arguments-error
                         '$vec4 "cannot convert two arguments into four $scalars"
                         "args" (list a b))]))]
    [(a b c)
     (apply make-$vec4
            (cond [(and (tvec2? a) (tvec1? b) (tvec1? c))
                   (map $scalar (list (tvec2-x a) (tvec2-y a) (tvec1-x b) (tvec1-x c)))]
                  [(and (tvec1? a) (tvec2? b) (tvec1? c))
                   (map $scalar (list (tvec1-x a) (tvec2-x b) (tvec2-y b) (tvec1-x c)))]
                  [(and (tvec1? a) (tvec1? b) (tvec2? c))
                   (map $scalar (list (tvec1-x a) (tvec1-x b) (tvec2-x c) (tvec2-y c)))]
                  [(tvec2? a) (map $scalar (list (tvec2-x a) (tvec2-y a) b c))]
                  [(tvec2? b) (map $scalar (list a (tvec2-x b) (tvec2-y b) c))]
                  [(tvec2? c) (map $scalar (list a b (tvec2-x c) (tvec2-y c)))]
                  [else (raise-arguments-error
                         '$vec4 "cannot convert three arguments into four $scalars"
                         "args" (list a b c))]))]
    [(a b c d)
     (apply make-$vec4
            (cond [(andmap $scalar? (list a b c d)) (list a b c d)]
                  [else (map $scalar (for/list ([A (in-list (list a b c d))])
                                       (if (tvec1? A) (tvec1-x A) A)))]))]))
