#lang racket/base

(require glm/private/types
         glm/scalar
         glm/vector/common
         glm/vector/types
         racket/contract
         racket/function
         template
         (for-syntax racket/base))

(begin-for-syntax
  (define-values (export-names export-ops)
    (values '(lessThan lessThanEqual greaterThan greaterThanEqual equal notEqual)
            '(< <= > >= = !=))))

(begin-template
  (provide #,@export-names
           any all not-
           (for*/template ([$ (in-list '(b d || i u))]
                           [N (in-list '(1 2 3 4))])
             (for/template ([OP (in-list export-names)]) OP/$vecN)
             any/$vecN
             all/$vecN)
           (for/template ([N (in-list '(1 2 3 4))]) not-/bvecN)))

(define != (negate =))

(define (// x1 x2)
  (or (!= x1 0) (!= x2 0)))

(define (&& x1 x2)
  (and (!= x1 0) (!= x2 0)))

(define (!! x)
  (!= x 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Definers

(define-template (define-relation/tvec OP R $ N)
  (define/contract (OP/$vecN v1 v2) (-> $vecN? $vecN? $vecN?)
    ($vecN (for/template ([X (in-list '(x y z w))]
                          [_ (in-range N)])
             (if (R ($vecN-X v1) ($vecN-X v2)) 1 0)))))

(define-template (define-bool-predicate/tvec OP C R $ N)
  (define/contract (OP/$vecN v) (-> $vecN? bscalar?)
    (if (C (for/template ([X (in-list '(x y z w))]
                          [_ (in-range N)])
             (!= ($vecN-X v) 0)))
        1 0)))

(define-template (define-unary-relation/bvec OP R N)
  (define/contract (OP/bvecN v) (-> bvecN? bvecN?)
    (bvecN (for/template ([X (in-list '(x y z w))]
                          [_ (in-range N)])
             (if (= (bvecN-X v) 0) 1 0)))))

(define-template (define-relation OP R)
  (for*/template ([$ (in-list '(b d || i u))]
                  [N (in-list '(1 2 3 4))])
    (define-relation/tvec OP R $ N))

  (define/contract (OP v1 v2) (-> tvec? tvec? tvec?)
    ((cond (for*/template ([$ (in-list '(b d || i u))]
                           [N (in-list '(1 2 3 4))])
             [($vecN? v1) OP/$vecN]))
     v1 v2)))

(define-template (define-bool-predicate OP C R)
  (for*/template ([$ (in-list '(b d || i u))]
                  [N (in-list '(1 2 3 4))])
    (define-bool-predicate/tvec OP C R $ N))

  (define/contract (OP v) (-> tvec? bscalar?)
    ((cond (for*/template ([$ (in-list '(b d || i u))]
                           [N (in-list '(1 2 3 4))])
             [($vecN? v) OP/$vecN]))
     v)))

(define-template (define-unary-relation OP R)
  (for/template ([N (in-list '(1 2 3 4))])
    (define-unary-relation/bvec OP R N))

  (define/contract (OP v) (-> bvec? bvec?)
    (cond (for/template ([N (in-list '(1 2 3 4))])
            [(bvecN? v) (OP/bvecN v)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Relations

(for/template ([OP (in-list export-names)]
               [R (in-list export-ops)])
  (define-relation OP R))

(define-bool-predicate any or //)
(define-bool-predicate all and &&)
(define-unary-relation not- !!)
