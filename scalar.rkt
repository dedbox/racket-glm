#lang racket/base

(require racket/bool
         racket/contract
         racket/function
         racket/flonum)

(provide (all-defined-out))

(define (tscalar? a) ((disjoin bscalar? dscalar? iscalar? uscalar?) a))
(define (bscalar? a) (and (real? a) (or (= a 0) (= a 1))))
(define (dscalar? a) (flonum? a))
(define (iscalar? a) (fixnum? a))
(define (uscalar? a) (and (fixnum? a) (not (negative? a))))

(define (fraction? a)
  (and (rational? a) (exact? a) (not (integer? a))))

(define (boolean x) (case x [(0 #f) #f] [else #t]))

(define/contract (bscalar a) (-> (or/c fixnum? flonum? fraction? boolean?) bscalar?)
  (case a [(0 #f) 0] [else 1]))

(define/contract (dscalar a) (-> (or/c fixnum? flonum? fraction? boolean?) dscalar?)
  (real->double-flonum
   (cond [(flonum? a) a]
         [(fixnum? a) (->fl a)]
         [(fraction? a) (->fl a)]
         [(boolean? a) (bscalar a)])))

(define/contract (iscalar a) (-> (or/c fixnum? flonum? fraction? boolean?) iscalar?)
  (cond [(fixnum? a) a]
        [(flonum? a) (fl->exact-integer (truncate a))]
        [(fraction? a) (fl->exact-integer (truncate (exact->inexact a)))]
        [(boolean? a) (bscalar a)]))

(define/contract (uscalar a) (-> (or/c fixnum? flonum? fraction? boolean?) iscalar?)
  (cond [(fixnum? a) ((if (negative? a) unsign values) a)]
        [(flonum? a) ((if (negative? a) unsign values) (fl->exact-integer (truncate a)))]
        [(fraction? a) ((if (negative? a) unsign values)
                        (fl->exact-integer (truncate (exact->inexact a))))]
        [(boolean? a) (bscalar a)]))

(define (unsign z)
  (integer-bytes->integer
   (integer->integer-bytes (bitwise-and #x3fffffffffffffff z) 8 #t)
   #f))

(define binteger values)

(define (dinteger a)
  (cond [(integer? a) (fl->exact-integer a)]
        [(real? a) (integer-bytes->integer (real->floating-point-bytes a 8) #t)]))

(define iinteger values)

(define (uinteger a)
  (cond [(negative? a) (integer-bytes->integer (integer->integer-bytes a 8) #f)]
        [else a]))
