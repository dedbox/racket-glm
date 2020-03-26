#lang racket/base

(require racket/bool
         racket/contract/base
         racket/match)

(provide (all-defined-out))

(define bscalar? (or/c 0 1))
(define bscalar (match-lambda [(or 0 #f) 0] [_ 1]))
(define boolean (match-lambda [0 #f] [1 #t]))

(define dscalar? flonum?)
(define (dscalar a)
  (real->double-flonum
   (cond [(false? a) 0]
         [ (real? a) a]
         [   else    1])))

(define scalar? flonum?)
(define (scalar a)
  (real->double-flonum
   (cond [(false? a) 0]
         [ (real? a) a]
         [   else    1])))

(define iscalar? exact-integer?)
(define (iscalar a)
  (cond [(false? a) 0]
        [(exact-integer? a) a]
        [(integer? a) (inexact->exact a)]
        [(real? a) (inexact->exact (truncate a))]
        [else 1]))

(define uscalar? exact-nonnegative-integer?)
(define (uscalar a)
  (cond [(false? a) 0]
        [(exact-nonnegative-integer? a) a]
        [(and (integer? a) (not (negative? a))) (inexact->exact a)]
        [(real? a) (define n (inexact->exact (truncate a)))
                   (integer-bytes->integer (integer->integer-bytes n 4 #t) #f)]
        [else 1]))

(define tscalar? (or/c bscalar? dscalar? scalar? iscalar? uscalar?))
