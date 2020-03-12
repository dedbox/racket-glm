#lang racket/base

(require template
         (for-syntax racket/base))

(provide (all-defined-out))

(struct tvec () #:transparent)
(struct tvec1 tvec (x) #:mutable #:transparent)
(struct tvec2 tvec (x y) #:mutable #:transparent)
(struct tvec3 tvec (x y z) #:mutable #:transparent)
(struct tvec4 tvec (x y z w) #:mutable #:transparent)

(for/template ([N (in-list '(1 2 3 4))])
  (for/template ([XYZW (in-list '(x y z w))]
                 [RGBA (in-list '(r g b a))]
                 [STPQ (in-list '(s t p q))]
                 [_ (in-range N)])
    (define tvecN-RGBA tvecN-XYZW)
    (define tvecN-STPQ tvecN-XYZW)
    (define set-tvecN-RGBA! set-tvecN-XYZW!)
    (define set-tvecN-STPQ! set-tvecN-XYZW!)))

(define (tvec-length v)
  (cond [(tvec1? v) 1]
        [(tvec2? v) 2]
        [(tvec3? v) 3]
        [(tvec4? v) 4]))

(define (arithmetic-rshift n m)
  (arithmetic-shift n (- m)))

(define binteger values)

(define (dinteger a)
  (cond [(integer? a) (inexact->exact a)]
        [(real? a) (integer-bytes->integer (real->floating-point-bytes a 8) #t)]))

(define (integer a)
  (cond [(integer? a) (inexact->exact a)]
        [(real? a) (integer-bytes->integer (real->floating-point-bytes a 4) #t)]))

(define iinteger values)

(define (uinteger a)
  (cond [(negative? a) (integer-bytes->integer (integer->integer-bytes a 8) #f)]
        [else a]))
