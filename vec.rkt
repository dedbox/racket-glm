#lang racket/base

;;; ----------------------------------------------------------------------------
;;; Abstract Vector

(require ffi/unsafe
         racket/flonum
         racket/function
         racket/generic
         racket/list
         racket/pretty
         racket/sequence
         (for-syntax racket/base
                     racket/syntax
                     syntax/transformer)
         (for-meta 2 racket/base))

(provide (all-defined-out))

(define (_vec len) (_array _float len))

(define-generics glm-vec
  (vec-name glm-vec)
  (vec-predicate glm-vec)
  (vec-constructor glm-vec))

(struct vec (data)
  #:name glm:vec
  #:constructor-name make-vec
  #:methods gen:equal+hash
  [(define/generic gen-hash-proc hash-proc)
   (define/generic gen-hash2-proc hash2-proc)
   (define (equal-proc v1 v2 _)
     (and (= (vec-length v1)
             (vec-length v2))
          (andmap = (vec->list v1) (vec->list v2))))
   (define (hash-proc v _) (gen-hash-proc (vec-data v)))
   (define (hash2-proc v _) (gen-hash2-proc (vec-data v)))]
  #:methods gen:custom-write
  [(define (write-proc v port mode)
     (case mode
       [(#t #f) (fprintf port "#<~a>" (vec-name v))]
       [(1 0) ((if (pretty-printing) pretty-print print)
               `(,(vec-name v) ,@(vec->list v))
               port mode)]))])

(define (make-vec-data . xs)
  (define len (length xs))
  (define data (ptr-ref (malloc (_vec len) 'atomic) (_vec len) 0))
  (for ([k (in-range len)]
        [x (in-list xs)])
    (array-set! data k x))
  data)

(define (vec-copy v)
  (apply (vec-constructor v) (vec->list v)))

(define (vec-length v)
  (array-length (vec-data v)))

(define (vec-ref v k)
  (array-ref (vec-data v) k))

(define (vec-set! v k x)
  (array-set! (vec-data v) k x))

(define (vec->list v)
  (sequence->list (in-array (vec-data v))))

(define (in-vec v)
  (in-array (vec-data v)))

(define (vec=! v w)
  (for ([k (in-range (vec-length v))]
        [wk (in-vec w)])
    (vec-set! v k wk)))

(define ((make-vec-binop op x0) . args)
  (define (vec-vec v1 v2)
    (apply (vec-constructor v1)
           (for/list ([x1 (in-vec v1)] [x2 (in-vec v2)]) (op x1 x2))))
  (define vec-binop
    (case-lambda
      [(v) (vec-binop ((vec-constructor v) x0) v)]
      [(a b) (cond [(and (vec? a) (vec? b)) (vec-vec a b)]
                   [(vec? a) (vec-vec a ((vec-constructor a) b))]
                   [(vec? b) (vec-vec ((vec-constructor b) a) b)]
                   [else (op a b)])]
      [(v . as) (for/fold ([w (vec-copy v)])
                          ([a (in-list as)])
                  (vec-binop w a))]))
  (apply vec-binop args))

(define vec+ (make-vec-binop fl+ 0.0))
(define vec- (make-vec-binop fl- 0.0))
(define vec* (make-vec-binop fl* 1.0))
(define vec/ (make-vec-binop fl/ 1.0))

(define (vec+= v a) (vec=! v (vec+ v a)))
(define (vec-= v a) (vec=! v (vec- v a)))
(define (vec*= v a) (vec=! v (vec* v a)))
(define (vec/= v a) (vec=! v (vec/ v a)))

(define (++vec v) (vec+= v ((vec-constructor v) 1.0)) v)
(define (--vec v) (vec-= v ((vec-constructor v) 1.0)) v)
(define (vec++ v) (begin0 (vec-copy v) (++vec v)))
(define (vec-- v) (begin0 (vec-copy v) (--vec v)))

;;; ============================================================================
;;; 
;;; 2-D Vector
;;;
;;; glm/detail/type_vec2.hpp
;;; glm/detail/type_vec2.inl

(struct vec2 glm:vec ()
  #:name glm:vec2
  #:constructor-name make-vec2
  #:methods gen:glm-vec
  [(define (vec-name _) 'vec2)
   (define (vec-predicate _) vec2?)
   (define (vec-constructor _) vec2)])

(define vec2
  (case-lambda
    [() (vec2 0.0)]
    [(a) (if (vec2? a) (vec-copy a) (vec2 a a))]
    [(x y) (make-vec2 (make-vec-data x y))]))

(define x-dir2 (vec2 1.0 0.0))
(define y-dir2 (vec2 0.0 1.0))
(define z-dir2 (vec2 0.0 0.0))
(define w-dir2 (vec2 0.0 0.0))

(define  zero2 (vec2 0.0 0.0))
(define   one2 (vec2 1.0 1.0))
(define   two2 (vec2 2.0 2.0))
(define three2 (vec2 3.0 3.0))
(define  four2 (vec2 4.0 4.0))
(define  five2 (vec2 5.0 5.0))
(define   six2 (vec2 6.0 6.0))
(define seven2 (vec2 7.0 7.0))
(define eight2 (vec2 8.0 8.0))
(define  nine2 (vec2 9.0 9.0))

(define   -one2 (vec2 -1.0 -1.0))
(define   -two2 (vec2 -2.0 -2.0))
(define -three2 (vec2 -3.0 -3.0))
(define  -four2 (vec2 -4.0 -4.0))
(define  -five2 (vec2 -5.0 -5.0))
(define   -six2 (vec2 -6.0 -6.0))
(define -seven2 (vec2 -7.0 -7.0))
(define -eight2 (vec2 -8.0 -8.0))
(define  -nine2 (vec2 -9.0 -9.0))

;;; ............................................................................

(module+ test
  (require racket/function
           rackunit)

  (test-case "equal?"
    (check equal? zero2 zero2)
    (check equal? one2 one2)
    (check (negate equal?) one2 zero2)
    (check (negate equal?) zero2 one2))

  (test-case "vec+="
    (define v (vec2 zero2))
    (check equal? v zero2) (vec+= v one2)
    (check equal? v one2) (vec+= v one2)
    (check equal? v two2))

  (test-case "vec-="
    (define v (vec2 one2))
    (check equal? v one2) (vec-= v one2)
    (check equal? v zero2) (vec-= v one2)
    (check equal? v -one2))

  (test-case "vec*="
    (define v (vec2 one2))
    (check equal? v one2) (vec*= v one2)
    (check equal? v one2) (vec*= v two2)
    (check equal? v two2) (vec*= v two2)
    (check equal? v four2))

  (test-case "vec/="
    (define v (vec2 one2))
    (check equal? v (vec2 1.0)) (vec/= v two2)
    (check equal? v (vec2 0.5)) (vec/= v five2)
    (check equal? v (vec2 0.1)))

  (test-case "vec+"
    (check equal? (vec+ one2) one2)
    (check equal? (vec+ one2 one2) two2)
    (check equal? (vec+ one2 one2 one2) three2))

  (test-case "vec-"
    (check equal? (vec- zero2) zero2)
    (check equal? (vec- one2 zero2) one2)
    (check equal? (vec- one2) -one2)
    (check equal? (vec- one2 one2) zero2)
    (check equal? (vec- one2 one2 one2) -one2)
    (check equal? (vec- one2 one2 one2 one2) -two2))

  (test-case "vec*"
    (check equal? (vec* zero2) zero2)
    (check equal? (vec* one2) one2)
    (check equal? (vec* two2) two2)
    (check equal? (vec* two2 three2) six2)
    (check equal? (vec* two2 three2 four2) (vec2 24.0 24.0))
    (check equal? (vec* (vec2 1.0 2.0)
                        (vec2 2.0 3.0)
                        (vec2 3.0 4.0)) (vec2 6.0 24.0)))

  (test-case "vec/"
    (check equal? (vec/ one2) one2)
    (check equal? (vec/ zero2 one2) zero2)
    (check equal? (vec/ one2 two2) (vec2 0.5 0.5))
    (check equal? (vec/ one2 two2 two2) (vec2 0.25 0.25))
    (check equal? (vec/ one2 two2 two2 two2) (vec2 0.125 0.125))))

;;; ============================================================================
;;; 
;;; 3-D Vector
;;;
;;; glm/detail/type_vec3.hpp
;;; glm/detail/type_vec3.inl

(struct vec3 glm:vec ()
  #:name glm:vec3
  #:constructor-name make-vec3
  #:methods gen:glm-vec
  [(define (vec-name _) 'vec3)
   (define (vec-predicate _) vec3?)
   (define (vec-constructor _) vec3)])

(define vec3
  (case-lambda
    [() (vec3 0.0)]
    [(a) (cond [(vec4? a) (apply vec3 (drop-right (vec->list a) 1))]
               [(vec3? a) (vec-copy a)]
               [else (vec3 a a a)])]
    [(x y z) (make-vec3 (make-vec-data x y z))]))

(define x-dir3 (vec3 1.0 0.0 0.0))
(define y-dir3 (vec3 0.0 1.0 0.0))
(define z-dir3 (vec3 0.0 0.0 1.0))
(define w-dir3 (vec3 0.0 0.0 0.0))

(define  zero3 (vec3 0.0 0.0 0.0))
(define   one3 (vec3 1.0 1.0 1.0))
(define   two3 (vec3 2.0 2.0 2.0))
(define three3 (vec3 3.0 3.0 3.0))
(define  four3 (vec3 4.0 4.0 4.0))
(define  five3 (vec3 5.0 5.0 5.0))
(define   six3 (vec3 6.0 6.0 6.0))
(define seven3 (vec3 7.0 7.0 7.0))
(define eight3 (vec3 8.0 8.0 8.0))
(define  nine3 (vec3 9.0 9.0 9.0))

(define   -one3 (vec3 -1.0 -1.0 -1.0))
(define   -two3 (vec3 -2.0 -2.0 -2.0))
(define -three3 (vec3 -3.0 -3.0 -3.0))
(define  -four3 (vec3 -4.0 -4.0 -4.0))
(define  -five3 (vec3 -5.0 -5.0 -5.0))
(define   -six3 (vec3 -6.0 -6.0 -6.0))
(define -seven3 (vec3 -7.0 -7.0 -7.0))
(define -eight3 (vec3 -8.0 -8.0 -8.0))
(define  -nine3 (vec3 -9.0 -9.0 -9.0))

;;; ............................................................................

(module+ test

  (test-case "equal?"
    (check equal? zero3 zero3)
    (check equal? one3 one3)
    (check (negate equal?) one3 zero3)
    (check (negate equal?) zero3 one3))

  (test-case "vec+="
    (define v (vec3 zero3))
    (check equal? v zero3) (vec+= v one3)
    (check equal? v one3) (vec+= v one3)
    (check equal? v two3))

  (test-case "vec-="
    (define v (vec3 one3))
    (check equal? v one3) (vec-= v one3)
    (check equal? v zero3) (vec-= v one3)
    (check equal? v -one3))

  (test-case "vec*="
    (define v (vec3 one3))
    (check equal? v one3) (vec*= v one3)
    (check equal? v one3) (vec*= v two3)
    (check equal? v two3) (vec*= v two3)
    (check equal? v four3))

  (test-case "vec/="
    (define v (vec3 one3))
    (check equal? v (vec3 1.0)) (vec/= v two3)
    (check equal? v (vec3 0.5)) (vec/= v five3)
    (check equal? v (vec3 0.1)))

  (test-case "vec+"
    (check equal? (vec+ one3) one3)
    (check equal? (vec+ one3 one3) two3)
    (check equal? (vec+ one3 one3 one3) three3))

  (test-case "vec-"
    (check equal? (vec- zero3) zero3)
    (check equal? (vec- one3 zero3) one3)
    (check equal? (vec- one3) -one3)
    (check equal? (vec- one3 one3) zero3)
    (check equal? (vec- one3 one3 one3) -one3)
    (check equal? (vec- one3 one3 one3 one3) -two3))

  (test-case "vec*"
    (check equal? (vec* zero3) zero3)
    (check equal? (vec* one3) one3)
    (check equal? (vec* two3) two3)
    (check equal? (vec* two3 three3) six3)
    (check equal? (vec* two3 three3 four3) (vec3 24.0 24.0 24.0))
    (check equal? (vec* (vec3 1.0 2.0 3.0)
                        (vec3 2.0 3.0 4.0)
                        (vec3 3.0 4.0 5.0)) (vec3 6.0 24.0 60.0)))

  (test-case "vec/"
    (check equal? (vec/ one3) one3)
    (check equal? (vec/ zero3 one3) zero3)
    (check equal? (vec/ one3 two3) (vec3 0.5 0.5 0.5))
    (check equal? (vec/ one3 two3 two3) (vec3 0.25 0.25 0.25))
    (check equal? (vec/ one3 two3 two3 two3) (vec3 0.125 0.125 0.125))))

;;; ============================================================================
;;; 
;;; 4-D Vector
;;;
;;; glm/detail/type_vec4.hpp
;;; glm/detail/type_vec4.inl

(struct vec4 glm:vec ()
  #:name glm:vec4
  #:constructor-name make-vec4
  #:methods gen:glm-vec
  [(define (vec-name _) 'vec4)
   (define (vec-predicate _) vec4?)
   (define (vec-constructor _) vec4)])

(define vec4
  (case-lambda
    [() (vec4 0.0)]
    [(a) (if (vec4? a) (vec-copy a) (vec4 a a a a))]
    [(a b) (cond [(vec3? a) (apply (curryr vec4 b) (vec->list a))]
                 [(vec3? b) (apply (curry vec4 a) (vec->list b))]
                 [else (raise-argument-error 'vec4 "a scalar or vec3" 0 a b)])]
    [(x y z w) (make-vec4 (make-vec-data x y z w))]))

(define x-dir4 (vec4 1.0 0.0 0.0 0.0))
(define y-dir4 (vec4 0.0 1.0 0.0 0.0))
(define z-dir4 (vec4 0.0 0.0 1.0 0.0))
(define w-dir4 (vec4 0.0 0.0 0.0 1.0))

(define-values (zero4 one4 two4 three4 four4 five4 six4 seven4 eight4 nine4)
  (apply values (for/list ([k (in-range 10)]) (vec4 (real->single-flonum k)))))

(define-values (-one4 -two4 -three4 -four4 -five4 -six4 -seven4 -eight4 -nine4)
  (apply values (for/list ([k (in-range 1 10)]) (vec4 (- (real->single-flonum k))))))

;;; ............................................................................

(module+ test

  ;; prototyping tests

  (test-case "equal?"
    (check equal? zero4 zero4)
    (check equal? one4 one4)
    (check (negate equal?) one4 zero4)
    (check (negate equal?) zero4 one4))

  (test-case "vec+="
    (define v (vec4 zero4))
    (check equal? v zero4) (vec+= v one4)
    (check equal? v one4) (vec+= v one4)
    (check equal? v two4))

  (test-case "vec-="
    (define v (vec4 one4))
    (check equal? v one4) (vec-= v one4)
    (check equal? v zero4) (vec-= v one4)
    (check equal? v -one4))

  (test-case "vec*="
    (define v (vec4 one4))
    (check equal? v one4) (vec*= v one4)
    (check equal? v one4) (vec*= v two4)
    (check equal? v two4) (vec*= v two4)
    (check equal? v four4))

  (test-case "vec/="
    (define v (vec4 one4))
    (check equal? v (vec4 1.0)) (vec/= v two4)
    (check equal? v (vec4 0.5)) (vec/= v five4)
    (check equal? v (vec4 0.1)))

  (test-case "vec+"
    (check equal? (vec+ one4) one4)
    (check equal? (vec+ one4 one4) two4)
    (check equal? (vec+ one4 one4 one4) three4))

  (test-case "vec-"
    (check equal? (vec- zero4) zero4)
    (check equal? (vec- one4 zero4) one4)
    (check equal? (vec- one4) -one4)
    (check equal? (vec- one4 one4) zero4)
    (check equal? (vec- one4 one4 one4) -one4)
    (check equal? (vec- one4 one4 one4 one4) -two4))

  (test-case "vec*"
    (check equal? (vec* zero4) zero4)
    (check equal? (vec* one4) one4)
    (check equal? (vec* two4) two4)
    (check equal? (vec* two4 three4) six4)
    (check equal? (vec* two4 three4 four4) (vec4 24.0 24.0 24.0 24.0))
    (check equal? (vec* (vec4 1.0 2.0 3.0 4.0)
                        (vec4 2.0 3.0 4.0 5.0)
                        (vec4 3.0 4.0 5.0 6.0)) (vec4 6.0 24.0 60.0 120.0)))

  (test-case "vec/"
    (check equal? (vec/ one4) one4)
    (check equal? (vec/ zero4 one4) zero4)
    (check equal? (vec/ one4 two4) (vec4 0.5 0.5 0.5 0.5))
    (check equal? (vec/ one4 two4 two4) (vec4 0.25 0.25 0.25 0.25))
    (check equal? (vec/ one4 two4 two4 two4) (vec4 0.125 0.125 0.125 0.125)))

  (test-case "vec++"
    (define v (vec4 zero4))
    (check equal? v zero4) (vec++ v)
    (check equal? v one4) (vec++ v)
    (check equal? v two4))

  (test-case "vec--"
    (define v (vec4 zero4))
    (check equal? v zero4) (vec-- v)
    (check equal? v -one4) (vec-- v)
    (check equal? v -two4))

  (test-case "vec+"
    (check equal? zero4 (vec+ zero4))
    (check equal? one4 (vec+ one4))
    (check equal? two4 (vec+ one4 1.0))
    (check equal? two4 (vec+ one4 one4))
    (check equal? three4 (vec+ one4 1.0 one4))
    (check equal? three4 (vec+ one4 two4)))

  (test-case "vec-"
    (check equal? zero4 (vec- zero4))
    (check equal? -one4 (vec- one4))
    (check equal? zero4 (vec- one4 1.0))
    (check equal? zero4 (vec- one4 one4))
    (check equal? -one4 (vec- one4 1.0 one4))
    (check equal? -one4 (vec- one4 two4)))

  (test-case "vec*"
    (check equal? zero4 (vec* zero4))
    (check equal? zero4 (vec* zero4 0.0))
    (check equal? zero4 (vec* zero4 zero4))
    (check equal? four4 (vec* two4 2.0))
    (check equal?  six4 (vec* two4 three4))
    (check equal?  six4 (vec* one4 2.0 three4)))

  (test-case "vec/"
    (check equal? one4 (vec/ one4))
    (check equal? two4 (vec/ six4 3.0))
    (check equal? two4 (vec/ six4 three4))
    (check equal? one4 (vec/ six4 3.0 two4))))
