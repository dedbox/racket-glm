#lang template ($ N ⊕ ⊗ ⊖ ⊘)

(require glm/private/vector-types
         glm/scalar
         racket/contract
         racket/flonum
         template
         (for-syntax racket/base))

;;; -----------------------------------------------------------------------------
;;; Non-Destructive Arithmetic

(for/template ([□ (in-list '(+ - * /))]
               [⊙ (in-list '(⊕ ⊗ ⊖ ⊘))])
  (define/contract ($vecN□ v a)
    (-> $vecN? (or/c (unless-template (= N 1) tvec1?) tvecN? $scalar?) $vecN?)
    ((cond [($scalar? a) $vecN□$scalar]
           [($vecN? a) $vecN□$vecN]
           (unless-template (= N 1) [(tvec1? a) $vecN□tvec1])
           [else $vecN□tvecN])
     v a))

  (define/contract ($vecN□$scalar v x) (-> $vecN $scalar? $vecN?)
    ($vecN (for/template ([X (in-list '(x y z w))]
                          [_ (in-range N)])
             ($scalar (⊙ (tvecN-X v) x)))))

  (define/contract ($vecN□$vecN v1 v2) (-> $vecN? $vecN? $vecN?)
    ($vecN (for/template ([X (in-list '(x y z w))]
                          [_ (in-range N)])
             ($scalar (⊙ (tvecN-X v1) (tvecN-X v2))))))

  (unless-template (= N 1)
    (define/contract ($vecN□tvec1 v1 v2) (-> $vecN? tvec1? $vecN?)
      (define x ($scalar (tvec1-x v2)))
      ($vecN (for/template ([X (in-list '(x y z w))]
                            [_ (in-range N)])
               ($scalar (⊙ (tvecN-X v1) x))))))

  (define/contract ($vecN□tvecN v1 v2) (-> $vecN? tvecN? $vecN?)
    ($vecN (for/template ([X (in-list '(x y z w))]
                          [_ (in-range N)])
             ($scalar (⊙ (tvecN-X v1) ($scalar (tvecN-X v2))))))))

;;; -----------------------------------------------------------------------------
;;; Non-Destructive Increment and Decrement

(define/contract ($vecN++ v) (-> $vecN? $vecN?)
  ($vecN (for/template ([X (in-list '(x y z w))]
                        [_ (in-range N)])
           ($scalar (⊕ (tvecN-X v) ($scalar 1))))))

(define/contract ($vecN-- v) (-> $vecN? $vecN?)
  ($vecN (for/template ([X (in-list '(x y z w))]
                        [_ (in-range N)])
           ($scalar (⊖ (tvecN-X v) ($scalar 1))))))
