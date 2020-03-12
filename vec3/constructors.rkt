#lang template ($)

(require glm/private/types
         racket/contract
         (for-syntax racket/base))

(struct $vec3 tvec3 ()
  #:transparent
  #:name glm-$vec3
  #:constructor-name make-$vec3)

(define/contract $vec3 (case-> (-> $vec3?)
                               (-> any/c $vec3?)
                               (-> any/c any/c $vec3?)
                               (-> any/c any/c any/c $vec3?))
  (case-lambda
    [() (apply make-$vec3 (build-list 3 (λ _ ($scalar 0))))]
    [(a)
     (apply make-$vec3
            (cond [($scalar? a) (list a a a)]
                  [($vec3? a) (list (tvec3-x a) (tvec3-y a) (tvec3-z a))]
                  [(tvec1? a) (build-list 3 (λ _ ($scalar (tvec1-x a))))]
                  [(tvec3? a) (map $scalar (list (tvec3-x a) (tvec3-y a) (tvec3-z a)))]
                  [(tvec4? a) (map $scalar (list (tvec4-x a) (tvec4-y a) (tvec3-z a)))]
                  [else (list ($scalar a) ($scalar a) ($scalar a))]))]
    [(a b)
     (apply make-$vec3
            (cond [(and (tvec2? a) (tvec1? b))
                   (map $scalar (list (tvec2-x a) (tvec2-y a) (tvec1-x b)))]
                  [(and (tvec1? a) (tvec2? b))
                   (map $scalar (list (tvec1-x a) (tvec2-x b) (tvec2-x b)))]
                  [(tvec2? a) (map $scalar (list (tvec2-x a) (tvec2-y a) b))]
                  [(tvec2? b) (map $scalar (list a (tvec2-x b) (tvec2-y b)))]
                  [else (raise-arguments-error
                         '$vec3 "cannot convert two arguments into three $scalars"
                         "args" (list a b))]))]
    [(a b c)
     (apply make-$vec3
            (cond [(andmap $scalar? (list a b c)) (list a b c)]
                  [(andmap tvec1? (list a b c)) (map $scalar (map tvec1-x (list a b c)))]
                  [(andmap tvec1? (list a b)) (map $scalar (list (tvec1-x a) (tvec1-x b) c))]
                  [(andmap tvec1? (list a c)) (map $scalar (list (tvec1-x a) b (tvec1-x c)))]
                  [(andmap tvec1? (list b c)) (map $scalar (list a (tvec1-x b) (tvec1-x c)))]
                  [(tvec1? a) (map $scalar (list (tvec1-x a) b c))]
                  [(tvec1? b) (map $scalar (list a (tvec1-x b) c))]
                  [(tvec1? c) (map $scalar (list a b (tvec1-x c)))]
                  [else (map $scalar (list a b c))]))]))
