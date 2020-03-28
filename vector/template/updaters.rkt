#lang template ($ N ⊕ ⊗ ⊖ ⊘)

(require glm/private/vector-types
         glm/scalar
         racket/contract
         racket/flonum
         template
         (for-syntax racket/base))

;;; -----------------------------------------------------------------------------
;;; Destructive Assignment

(define/contract ($vecN=! v1 v2) (-> $vecN? tvecN? $vecN?)
  ((if ($vecN? v2) $vecN=$vecN! $vecN=tvecN!) v1 v2))

(define/contract ($vecN=tvecN! v1 v2) (-> $vecN? tvecN? $vecN?)
  (update-tvecN! X v1 ($scalar (tvecN-X v2)))
  v1)

(define/contract ($vecN=$vecN! v1 v2) (-> $vecN? $vecN? $vecN?)
  (update-tvecN! X v1 (tvecN-X v2))
  v1)

;;; -----------------------------------------------------------------------------
;;; Destructive Arithmetic

(for/template ([□ (in-list '(+ - * /))]
               [⊙ (in-list '(⊕ ⊗ ⊖ ⊘))])
  (define/contract ($vecN□=! v a) (-> $vecN? (or/c tvecN? $scalar?) $vecN?)
    ((cond [($scalar? a) $vecN□=$scalar!]
           [($vecN? a) $vecN□=$vecN!]
           (unless-template (= N 1)
             [(tvec1? a) $vecN□=tvec1!])
           [else $vecN□=tvecN!])
     v a))

  (define/contract ($vecN□=$scalar! v x) (-> $vecN $scalar? $vecN?)
    (update-tvecN! X v ($scalar (⊙ (tvecN-X v) x)))
    v)

  (define/contract ($vecN□=$vecN! v1 v2) (-> $vecN? $vecN? $vecN?)
    (update-tvecN! X v1 ($scalar (⊙ (tvecN-X v1) (tvecN-X v2))))
    v1)

  (unless-template (= N 1)
    (define/contract ($vecN□=tvec1! v1 v2) (-> $vecN? tvec1? $vecN?)
      (define x ($scalar (tvec1-x v2)))
      (update-tvecN! X v1 ($scalar (⊙ (tvecN-X v1) x)))
      v1))

  (define/contract ($vecN□=tvecN! v1 v2) (-> $vecN? tvecN? $vecN?)
    (update-tvecN! X v1 ($scalar (⊙ (tvecN-X v1) ($scalar (tvecN-X v2)))))
    v1))

;;; -----------------------------------------------------------------------------
;;; Destructive Increment and Decrement

(define/contract ($vecN++! v) (-> $vecN? $vecN?)
  (update-tvecN! X v ($scalar (⊕ (tvecN-X v) ($scalar 1))))
  v)

(define/contract ($vecN--! v) (-> $vecN? $vecN?)
  (update-tvecN! X v ($scalar (⊖ (tvecN-X v) ($scalar 1))))
  v)
