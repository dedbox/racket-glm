#lang template ($ N)

(require glm/private/vector-types
         glm/scalar
         racket/contract
         racket/list
         (for-syntax racket/base))

(define/contract ($vecN . as)
  (case-> (for/template ([K (in-range (add1 N))])
            (-> (for/template ([_ (in-range K)]) targ?) $vecN?)))
  (define xs (args->$scalars as))
  (cond [(= (length xs) N) (apply make-$vecN xs)]
        [(= (length xs) 1) (apply make-$vecN (build-list N (λ _ (car xs))))]
        [(> (length xs) N) (apply make-$vecN (take xs N))]
        [else
         (raise-arguments-error
          '$vecN "cannot convert arguments into N components"
          "arguments" as "components" xs)]))
