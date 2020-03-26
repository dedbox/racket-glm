#lang template ($ N)

(require glm/private/vector-types
         glm/scalar
         racket/contract
         (for-syntax racket/base))

(cond-template
  [(= N 1)
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
                              [else ($scalar a)]))]))]

  [(= N 2)
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
                     [else (list ($scalar a) ($scalar b))]))]))]

  [(= N 3)
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
        (apply
         make-$vec3
         (cond [($scalar? a) (list a a a)]
               [($vec3? a) (list (tvec3-x a) (tvec3-y a) (tvec3-z a))]
               [(tvec1? a) (build-list 3 (λ _ ($scalar (tvec1-x a))))]
               [(tvec3? a) (map $scalar (list (tvec3-x a) (tvec3-y a) (tvec3-z a)))]
               [(tvec4? a) (map $scalar (list (tvec4-x a) (tvec4-y a) (tvec4-z a)))]
               [else (list ($scalar a) ($scalar a) ($scalar a))]))]
       [(a b)
        (apply
         make-$vec3
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
        (apply
         make-$vec3
         (cond
           [(andmap $scalar? (list a b c)) (list a b c)]
           [(andmap tvec1? (list a b c)) (map $scalar (map tvec1-x (list a b c)))]
           [(andmap tvec1? (list a b)) (map $scalar (list (tvec1-x a) (tvec1-x b) c))]
           [(andmap tvec1? (list a c)) (map $scalar (list (tvec1-x a) b (tvec1-x c)))]
           [(andmap tvec1? (list b c)) (map $scalar (list a (tvec1-x b) (tvec1-x c)))]
           [(tvec1? a) (map $scalar (list (tvec1-x a) b c))]
           [(tvec1? b) (map $scalar (list a (tvec1-x b) c))]
           [(tvec1? c) (map $scalar (list a b (tvec1-x c)))]
           [else (map $scalar (list a b c))]))]))]

  [(= N 4)
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
        (apply
         make-$vec4
         (cond [($scalar? a) (list a a a a)]
               [($vec4? a) (list (tvec4-x a) (tvec4-y a) (tvec4-z a) (tvec4-w a))]
               [(tvec1? a) (build-list 4 (λ _ ($scalar (tvec1-x a))))]
               [(tvec4? a) (map $scalar (list (tvec4-x a) (tvec4-y a) (tvec4-z a)))]
               [else (build-list 4 (λ _ ($scalar a)))]))]
       [(a b)
        (apply
         make-$vec4
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
        (apply
         make-$vec4
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
                                          (if (tvec1? A) (tvec1-x A) A)))]))]))])
