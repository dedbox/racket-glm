#lang template ($ N ⊕ ⊖ ⊗ ⊘ ≗)

(require glm/private/ops
         glm/private/types
         racket/contract
         racket/flonum
         (for-syntax racket/base))

;;; Component Access

(define/contract ($vecN-ref this i)
  (-> $vecN? (or/c (for/template ([K (in-range N)]) K)) $scalar?)
  ((case i (for/template ([X (in-list '(x y z w))]
                          [K (in-range N)])
             [(K) tvecN-X]))
   this))

(for/template ([X (in-list '(x r s))]
               [Y (in-list '(y g t))]
               [Z (in-list '(z b p))]
               [W (in-list '(w a q))])
  (for/template ([C (in-list '(X Y Z W))]
                 [_ (in-range N)])
    (define/contract $vecN-C (-> $vecN? $scalar?) (procedure-rename tvecN-C '$vecN-C))
    (define/contract set-$vecN-C! (-> $vecN? $scalar? $vecN?)
      (procedure-rename set-tvecN-C! 'set-$vecN-C!))))

;;; Assignment

(define/contract ($vecN=$vecN! this v) (-> $vecN? $vecN? $vecN?)
  (for/template ([X (in-list '(x y z w))]
                 [_ (in-range N)])
    (set-tvecN-X! this (tvecN-X v)))
  this)

(define/contract ($vecN=tvecN! this v) (-> $vecN? tvecN? $vecN?)
  (for/template ([X (in-list '(x y z w))]
                 [_ (in-range N)])
    (set-tvecN-X! this ($scalar (tvecN-X v))))
  this)

(define/contract ($vecN=! this a) (-> $vecN? tvecN? $vecN?)
  ((if ($vecN? a) $vecN=$vecN! $vecN=tvecN!) this a))

;;; Operator Definitions

(define-template (define-$vecN-unop ⊙ OP)
  (define/contract ($vecN⊙=$scalar! this x) (-> $vecN? $scalar? $vecN?)
    (for/template ([X (in-list '(x y z w))]
                   [_ (in-range N)])
      (set-tvecN-X! this ($scalar (OP (tvecN-X this) x))))
    this)

  (define/contract ($vecN⊙=any! this a) (-> $vecN any/c $vecN?)
    (for/template ([X (in-list '(x y z w))]
                   [_ (in-range N)])
      (set-tvecN-X! this ($scalar (OP (tvecN-X this) ($scalar a)))))
    this)

  (define/contract ($vecN⊙=$vecN! this v) (-> $vecN? $vecN? $vecN?)
    (for/template ([X (in-list '(x y z w))]
                   [_ (in-range N)])
      (set-tvecN-X! this ($scalar (OP (tvecN-X this) (tvecN-X v)))))
    this)

  (unless-template (= N 1)
    (define/contract ($vecN⊙=tvec1! this v) (-> $vecN? tvec1? $vecN?)
      (for/template ([X (in-list '(x y z w))]
                     [_ (in-range N)])
        (set-tvecN-X! this ($scalar (OP (tvecN-X this) ($scalar (tvec1-x v))))))
      this))

  (define/contract ($vecN⊙=tvecN! this v) (-> $vecN? tvecN? $vecN?)
    (for/template ([X (in-list '(x y z w))]
                   [_ (in-range N)])
      (set-tvecN-X! this ($scalar (OP (tvecN-X this) ($scalar (tvecN-X v))))))
    this)

  (define/contract ($vecN⊙=! this a) (-> $vecN? any/c $vecN?)
    ((cond [($scalar? a) $vecN⊙=$scalar!]
           [($vecN? a) $vecN⊙=$vecN!]
           (unless-template (= N 1) [(tvec1? a) $vecN⊙=tvec1!])
           [(tvecN? a) $vecN⊙=tvecN!]
           [else $vecN⊙=any!])
     this a)))

(define-template (define-$vecN-binop ⊙ OP)
  (define/contract ($vecN⊙$scalar v x) (-> $vecN? $scalar? $vecN?)
    ($vecN (for/template ([X (in-list '(x y z w))]
                          [_ (in-range N)])
             ($scalar (OP (tvecN-X v) x)))))

  (define/contract ($vecN⊙any v a) (-> $vecN? any/c $vecN?)
    ($vecN (for/template ([X (in-list '(x y z w))]
                          [_ (in-range N)])
             ($scalar (OP (tvecN-X v) ($scalar a))))))

  (define/contract ($vecN⊙$vecN v1 v2) (-> $vecN? $vecN? $vecN?)
    ($vecN (for/template ([X (in-list '(x y z w))]
                          [_ (in-range N)])
             ($scalar (OP (tvecN-X v1) (tvecN-X v2))))))

  (unless-template (= N 1)
    (define/contract ($vecN⊙tvec1 v1 v2) (-> $vecN? tvec1? $vecN?)
      ($vecN (for/template ([X (in-list '(x y z w))]
                            [_ (in-range N)])
               ($scalar (OP (tvecN-X v1) ($scalar (tvec1-x v2))))))))

  (define/contract ($vecN⊙tvecN v1 v2) (-> $vecN? tvecN? $vecN?)
    ($vecN (for/template ([X (in-list '(x y z w))]
                          [_ (in-range N)])
             ($scalar (OP (tvecN-X v1) ($scalar (tvecN-X v2)))))))

  (define/contract ($vecN⊙ v a) (-> $vecN? any/c $vecN?)
    ((cond [($scalar? a) $vecN⊙$scalar]
           [($vecN? a) $vecN⊙$vecN]
           (unless-template (= N 1) [(tvec1? a) $vecN⊙tvec1])
           [(tvecN? a) $vecN⊙tvecN]
           [else $vecN⊙any])
     v a)))

;;; Arithmetic Operators

(for/template ([⊙ (in-list '(+ - * /))]
               [OP (in-list '(⊕ ⊖ ⊗ ⊘))])
  (define-$vecN-unop ⊙ OP)
  (define-$vecN-binop ⊙ OP))

;;; Increment and Decrement Operators

(define/contract ($vecN++! this) (-> $vecN? $vecN?)
  (for/template ([X (in-list '(x y z w))]
                 [_ (in-range N)])
    (set-tvecN-X! this ($scalar (⊕ (tvecN-X this) ($scalar 1)))))
  this)

(define/contract ($vecN--! this) (-> $vecN? $vecN?)
  (for/template ([X (in-list '(x y z w))]
                 [_ (in-range N)])
    (set-tvecN-X! this ($scalar (⊖ (tvecN-X this) ($scalar 1)))))
  this)

;;; Bitwise Operators

(for/template ([⊙ (in-list '(% & // ^ << >>))]
               [OP (in-list '($mod* $and* $or* $xor* $lshift* $rshift*))])
  (define-$vecN-unop ⊙ OP)
  (define-$vecN-binop ⊙ OP))

(define/contract ($vecN~ v) (-> $vecN? $vecN?)
  ($vecN (for/template ([X (in-list '(x y z w))]
                        [_ (in-range N)])
           ($not* (tvecN-X v)))))

;;; Boolean Operators

(define/contract ($vecN=? v1 v2) (-> $vecN? $vecN? boolean?)
  (and (for/template ([X (in-list '(x y z w))]
                      [_ (in-range N)])
         (≗ (tvecN-X v1) (tvecN-X v2)))))
