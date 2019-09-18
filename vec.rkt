#lang racket/base

;;; ----------------------------------------------------------------------------
;;; Vector Base

(require ffi/unsafe
         racket/flonum
         racket/function
         racket/generic
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
