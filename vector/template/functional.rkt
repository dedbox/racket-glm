#lang template ($ N ⊕ ⊗ ⊖ ⊘ ≗)

(require glm/private/vector-ops
         glm/private/vector-types
         glm/scalar
         racket/contract
         racket/fixnum
         racket/flonum
         template
         (for-syntax racket/base))

;;; -----------------------------------------------------------------------------
;;; Functional (Non-Destructive) Operations

(for/template ([□ (in-list '(+ - * / % & // ^ << >>))]
               [⊙ (in-list '(⊕ ⊗ ⊖ ⊘ $mod* $and* $or* $xor* $lshift* $rshift*))])
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

;;; Bitwise Operations

(define/contract ($vecN~ v) (-> $vecN? $vecN?)
  ($vecN (for/template ([X (in-list '(x y z w))]
                        [_ (in-range N)])
           ($not* (tvecN-X v)))))

;;; Logical Operations

(define/contract ($vecN=? v1 v2) (-> $vecN? $vecN? boolean?)
  (and (for/template ([X (in-list '(x y z w))]
                      [_ (in-range N)])
         (≗ (tvecN-X v1) (tvecN-X v2)))))

;;; Increment and Decrement

(define/contract ($vecN++ v) (-> $vecN? $vecN?)
  ($vecN (for/template ([X (in-list '(x y z w))]
                        [_ (in-range N)])
           ($scalar (⊕ (tvecN-X v) ($scalar 1))))))

(define/contract ($vecN-- v) (-> $vecN? $vecN?)
  ($vecN (for/template ([X (in-list '(x y z w))]
                        [_ (in-range N)])
           ($scalar (⊖ (tvecN-X v) ($scalar 1))))))
