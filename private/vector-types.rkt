#lang racket/base

(require glm/scalar
         racket/contract
         racket/list
         template
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
    (define/contract set-tvecN-R! (-> tvecN? tscalar? void?) set-tvecN-X!)
    (define/contract set-tvecN-S! (-> tvecN? tscalar? void?) set-tvecN-X!)))

(for/template ([N (in-list '(1 2 3 4))])
  (define-syntax (update-tvecN! stx)
    (syntax-case stx ()
      [(_ X v expr)
       (with-syntax ([X (syntax-local-introduce #'X)]
                     [v (syntax-local-introduce #'v)]
                     [expr (syntax-local-introduce #'expr)])
         (quote-template ()
           #'(for/template ([X (in-list '(x y z w))]
                            [_ (in-range N)])
               (set-tvecN-X! v expr))))])))

(for*/template ([$ (in-list '(b d || i u))]
                [N (in-list '(1 2 3 4))])
  (struct $vecN tvecN ()
    #:transparent
    #:name glm-$vecN
    #:constructor-name make-$vecN))

(define targ? (or/c number? tscalar? tvec?))

(for/template ([$ (in-list '(b d || i u))])
  (define (args->$scalars as)
    (define (arg->scalars a)
      (cond [($scalar? a) (list a)]
            [(number? a) (list ($scalar a))]
            [(tvec1? a) (list ($scalar (tvec1-x a)))]
            [(tvec2? a) (list ($scalar (tvec2-x a))
                              ($scalar (tvec2-y a)))]
            [(tvec3? a) (list ($scalar (tvec3-x a))
                              ($scalar (tvec3-y a))
                              ($scalar (tvec3-z a)))]
            [(tvec4? a) (list ($scalar (tvec4-x a))
                              ($scalar (tvec4-y a))
                              ($scalar (tvec4-z a))
                              ($scalar (tvec4-w a)))]))
    (cond [(null? as) (list ($scalar 0))]
          [(null? (cdr as)) (arg->scalars (car as))]
          [else (flatten (map arg->scalars as))])))
