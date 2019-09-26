#lang racket/base

(require (except-in ffi/unsafe ->)
         racket/contract
         racket/flonum
         racket/function
         racket/generic
         racket/list
         racket/pretty
         racket/sequence
         (for-syntax racket/base
                     racket/syntax))

(provide (all-defined-out))

(define (_vec len) (_array _float len))

(define/contract (scalar a) (-> number? single-flonum?)
  (real->single-flonum (if (exact? a) (exact->inexact a) a)))

(struct vec (data length)
  #:transparent
  #:name glm:vec
  #:constructor-name make-vec
  #:methods gen:equal+hash
  [(define/generic gen-hash-proc hash-proc)
   (define/generic gen-hash2-proc hash2-proc)
   (define (equal-proc v1 v2 _)
     (and (= (vec-length v1) (vec-length v2))
          (for/and ([x1 (in-vec v1)]
                    [x2 (in-vec v2)])
            (= x1 x2))))
   (define (hash-proc v _) (gen-hash-proc (vec-data v)))
   (define (hash2-proc v _) (gen-hash2-proc (vec-data v)))]
  #:methods gen:custom-write
  [(define (write-proc v port mode)
     (case mode
       [(#t #f) (fprintf port "#<vec~a>" (vec-length v))]
       [(1 0)
        (define printer (if (pretty-printing) pretty-print print))
        (printer `(,@(if (<= (vec-length v) 4)
                         (list (vec-name v))
                         `(vec #:length ,(vec-length v)))
                   ,@(vec->list v))
                 port mode)]))])

(define/contract (vec #:length [len #f] . as)
  (->* () (#:length (or/c exact-nonnegative-integer? #f))
       #:rest (listof (or/c vec? number?))
       vec?)
  (define xs
    (apply append (for/list ([a (in-list as)])
                    (if (vec? a) (apply to-scalars (vec->list a)) (list a)))))
  (when len
    (define xs-len (length xs))
    (set! xs
      (cond
        [(= xs-len 0) (build-list len (λ _ 0))]
        [(= xs-len 1) (build-list len (λ _ (car xs)))]
        [(= xs-len len) xs]
        [(> xs-len len) (take xs len)]
        [(< xs-len len) (append xs (build-list (- len (length xs)) (λ _ 0)))])))
  (define data (apply make-vec-data xs))
  (make-vec data (array-length data)))

(define to-scalars
  (case-lambda
    [() null]
    [(a . as) (if (vec? a)
                  (append (vec->list a) (apply to-scalars as))
                  (cons a (apply to-scalars as)))]))

(define/contract (make-vec-data . args)
  (->* (number?) #:rest (listof number?) array?)
  (define len (length args))
  (define data (ptr-ref (malloc (_vec len) 'atomic) (_vec len) 0))
  (for ([k (in-range len)]
        [x (in-list (map scalar args))])
    (array-set! data k x))
  data)

(define/contract (vec-copy v)
  (-> vec? vec?)
  (apply vec (vec->list v)))

(define/contract (vec-name v)
  (-> vec? symbol?)
  (string->symbol (format "vec~a" (vec-length v))))

(define/contract ((vec-predicate v) a)
  (-> vec? predicate/c)
  (and (vec? a) (= (vec-length a) length)))

(define (vec-ref v i)
  (array-ref (vec-data v) i))

(define (vec-set! v i x)
  (array-set! (vec-data v) i (scalar x)))

(define/contract (vec->list v)
  (-> vec? (listof flonum?))
  (sequence->list (in-array (vec-data v))))

(define/contract (in-vec v)
  (-> vec? sequence?)
  (in-array (vec-data v)))

(define/contract (vec-constructor v)
  (-> vec? (unconstrained-domain-> vec?))
  (curry vec #:length (vec-length v)))

(define/contract (vec=! v u)
  (-> vec? vec? void?)
  (for ([i (in-range (vec-length v))]
        [ui (in-vec u)])
    (vec-set! v i ui)))

(define/contract ((make-vec-binop op x0) . args)
  (-> (-> number? number? number?)
      number?
      (unconstrained-domain-> (or/c vec? number?)))
  (define (vec-vec v1 v2)
    (apply (vec-constructor v1)
           (for/list ([x1 (in-vec v1)]
                      [x2 (in-vec v2)])
             (op x1 x2))))
  (define vec-binop
    (case-lambda
      [(a) (if (vec? a)
               (vec-binop ((vec-constructor a) (scalar x0)) a)
               (op x0 a))]
      [(a b) (cond [(and (vec? a) (vec? b)) (vec-vec a b)]
                   [(vec? a) (vec-vec a ((vec-constructor a) b))]
                   [(vec? b) (vec-vec ((vec-constructor b) a) b)]
                   [else (op a b)])]
      [(a b . cs) (apply vec-binop (vec-binop a b) cs)]))
  (apply vec-binop args))

(define vec+ (make-vec-binop + 0))
(define vec- (make-vec-binop - 0))
(define vec* (make-vec-binop * 1))
(define vec/ (make-vec-binop / 1))

(define (vec+= v a) (vec=! v (vec+ v a)))
(define (vec-= v a) (vec=! v (vec- v a)))
(define (vec*= v a) (vec=! v (vec* v a)))
(define (vec/= v a) (vec=! v (vec/ v a)))

(define (++vec v) (vec+= v ((vec-constructor v) 1.0)) v)
(define (--vec v) (vec-= v ((vec-constructor v) 1.0)) v)
(define (vec++ v) (begin0 (vec-copy v) (++vec v)))
(define (vec-- v) (begin0 (vec-copy v) (--vec v)))

;;; ----------------------------------------------------------------------------

(define-syntax (define-vec stx)
  (syntax-case stx ()
    [(_ type-id len)
     (identifier? #'type-id)
     (with-syntax ([type-id? (format-id #'type-id "~a?" #'type-id)])
       #'(begin
           (define/contract (type-id? a)
             predicate/c
             (and (vec? a) (= (vec-length a) len)))
           (define type-id (curry vec #:length len))))]))

(define-vec vec1 1)
(define-vec vec2 2)
(define-vec vec3 3)
(define-vec vec4 4)

;;; ============================================================================

(module+ test
  (require racket/function
           rackunit)

  (test-case "equal?"
    (check equal? (vec2 0) (vec2 0))
    (check equal? (vec2 1) (vec2 1))
    (check (negate equal?) (vec2 1) (vec2 0))
    (check (negate equal?) (vec2 0) (vec2 1)))

  (test-case "vec+="
    (define v
      (vec2 (vec2 0))) (check equal? v (vec2 0))
    (vec+= v (vec2 1)) (check equal? v (vec2 1))
    (vec+= v (vec2 1)) (check equal? v (vec2 2)))

  (test-case "vec-="
    (define v
      (vec2 (vec2 1))) (check equal? v (vec2 1))
    (vec-= v (vec2 1)) (check equal? v (vec2 0))
    (vec-= v (vec2 1)) (check equal? v (vec2 -1)))

  (test-case "vec*="
    (define v
      (vec2 (vec2 1))) (check equal? v (vec2 1))
    (vec*= v (vec2 1)) (check equal? v (vec2 1))
    (vec*= v (vec2 2)) (check equal? v (vec2 2))
    (vec*= v (vec2 2)) (check equal? v (vec2 4)))

  (test-case "vec/="
    (define v
      (vec2 (vec2 1))) (check equal? v (vec2  1 ))
    (vec/= v (vec2 2)) (check equal? v (vec2 1/2))
    (vec/= v (vec2 5)) (check equal? v (vec2 0.1)))

  (test-case "vec+"
    (check equal? (vec+ (vec2 1)) (vec2 1))
    (check equal? (vec+ (vec2 1) (vec2 1)) (vec2 2))
    (check equal? (vec+ (vec2 1) (vec2 1) (vec2 1)) (vec2 3)))

  (test-case "vec-"
    (check equal? (vec- (vec2 0)) (vec2 0))
    (check equal? (vec- (vec2 1) (vec2 0)) (vec2 1))
    (check equal? (vec- (vec2 1)) (vec2 -1))
    (check equal? (vec- (vec2 1) (vec2 1)) (vec2 0))
    (check equal? (vec- (vec2 1) (vec2 1) (vec2 1)) (vec2 -1))
    (check equal? (vec- (vec2 1) (vec2 1) (vec2 1) (vec2 1)) (vec2 -2)))

  (test-case "vec*"
    (check equal? (vec* (vec2 0)) (vec2 0))
    (check equal? (vec* (vec2 1)) (vec2 1))
    (check equal? (vec* (vec2 2)) (vec2 2))
    (check equal? (vec* (vec2 2) (vec2 3)) (vec2 6))
    (check equal? (vec* (vec2 2) (vec2 3) (vec2 4)) (vec2 24 24))
    (check equal? (vec* (vec2 1 2) (vec2 2 3) (vec2 3 4)) (vec2 6 24)))

  (test-case "vec/"
    (check equal? (vec/ (vec2 1)) (vec2 1))
    (check equal? (vec/ (vec2 0) (vec2 1)) (vec2 0))
    (check equal? (vec/ (vec2 1) (vec2 2)) (vec2 1/2 1/2))
    (check equal? (vec/ (vec2 1) (vec2 2) (vec2 2)) (vec2 1/4 1/4))
    (check equal? (vec/ (vec2 1) (vec2 2) (vec2 2) (vec2 2)) (vec2 1/8 1/8)))

  ;;; ..........................................................................

  (test-case "equal?"
    (check equal? (vec2 0) (vec2 0))
    (check equal? (vec2 1) (vec2 1))
    (check (negate equal?) (vec2 1) (vec2 0))
    (check (negate equal?) (vec2 0) (vec2 1)))

  (test-case "vec+="
    (define v
      (vec2 (vec2 0))) (check equal? v (vec2 0))
    (vec+= v (vec2 1)) (check equal? v (vec2 1))
    (vec+= v (vec2 1)) (check equal? v (vec2 2)))

  (test-case "vec-="
    (define v
      (vec2 (vec2 1))) (check equal? v (vec2 1))
    (vec-= v (vec2 1)) (check equal? v (vec2 0))
    (vec-= v (vec2 1)) (check equal? v (vec2 -1)))

  (test-case "vec*="
    (define v
      (vec2 (vec2 1))) (check equal? v (vec2 1))
    (vec*= v (vec2 1)) (check equal? v (vec2 1))
    (vec*= v (vec2 2)) (check equal? v (vec2 2))
    (vec*= v (vec2 2)) (check equal? v (vec2 4)))

  (test-case "vec/="
    (define v (vec2 (vec2 1)))
    (check equal? v (vec2  1 )) (vec/= v (vec2 2))
    (check equal? v (vec2 1/2)) (vec/= v (vec2 5))
    (check equal? v (vec2 0.1)))

  (test-case "vec+"
    (check equal? (vec+ (vec2 1)) (vec2 1))
    (check equal? (vec+ (vec2 1) (vec2 1)) (vec2 2))
    (check equal? (vec+ (vec2 1) (vec2 1) (vec2 1)) (vec2 3)))

  (test-case "vec-"
    (check equal? (vec- (vec2 0)) (vec2 0))
    (check equal? (vec- (vec2 1) (vec2 0)) (vec2 1))
    (check equal? (vec- (vec2 1)) (vec2 -1))
    (check equal? (vec- (vec2 1) (vec2 1)) (vec2 0))
    (check equal? (vec- (vec2 1) (vec2 1) (vec2 1)) (vec2 -1))
    (check equal? (vec- (vec2 1) (vec2 1) (vec2 1) (vec2 1)) (vec2 -2)))

  (test-case "vec*"
    (check equal? (vec* (vec2 0)) (vec2 0))
    (check equal? (vec* (vec2 1)) (vec2 1))
    (check equal? (vec* (vec2 2)) (vec2 2))
    (check equal? (vec* (vec2 2) (vec2 3)) (vec2 6))
    (check equal? (vec* (vec2 2) (vec2 3) (vec2 4)) (vec2 24))
    (check equal? (vec* (vec2 1 2) (vec2 2 3) (vec2 3 4)) (vec2 6 24)))

  (test-case "vec/"
    (check equal? (vec/ (vec2 1)) (vec2 1))
    (check equal? (vec/ (vec2 0) (vec2 1)) (vec2 0))
    (check equal? (vec/ (vec2 1) (vec2 2)) (vec2 1/2))
    (check equal? (vec/ (vec2 1) (vec2 2) (vec2 2)) (vec2 1/4))
    (check equal? (vec/ (vec2 1) (vec2 2) (vec2 2) (vec2 2)) (vec2 1/8)))

  ;;; ..........................................................................

  (test-case "equal?"
    (check equal? (vec3 0) (vec3 0))
    (check equal? (vec3 1) (vec3 1))
    (check (negate equal?) (vec3 1) (vec3 0))
    (check (negate equal?) (vec3 0) (vec3 1)))

  (test-case "vec+="
    (define v
      (vec3 (vec3 0))) (check equal? v (vec3 0))
    (vec+= v (vec3 1)) (check equal? v (vec3 1))
    (vec+= v (vec3 1)) (check equal? v (vec3 2)))

  (test-case "vec-="
    (define v
      (vec3 (vec3 1))) (check equal? v (vec3 1))
    (vec-= v (vec3 1)) (check equal? v (vec3 0))
    (vec-= v (vec3 1)) (check equal? v (vec3 -1)))

  (test-case "vec*="
    (define v
      (vec3 (vec3 1))) (check equal? v (vec3 1))
    (vec*= v (vec3 1)) (check equal? v (vec3 1))
    (vec*= v (vec3 2)) (check equal? v (vec3 2))
    (vec*= v (vec3 2)) (check equal? v (vec3 4)))

  (test-case "vec/="
    (define v
      (vec3 (vec3 1))) (check equal? v (vec3  1 ))
    (vec/= v (vec3 2)) (check equal? v (vec3 1/2))
    (vec/= v (vec3 5)) (check equal? v (vec3 0.1)))

  (test-case "vec+"
    (check equal? (vec+ (vec3 1)) (vec3 1))
    (check equal? (vec+ (vec3 1) (vec3 1)) (vec3 2))
    (check equal? (vec+ (vec3 1) (vec3 1) (vec3 1)) (vec3 3)))

  (test-case "vec-"
    (check equal? (vec- (vec3 0)) (vec3 0))
    (check equal? (vec- (vec3 1) (vec3 0)) (vec3 1))
    (check equal? (vec- (vec3 1)) (vec3 -1))
    (check equal? (vec- (vec3 1) (vec3 1)) (vec3 0))
    (check equal? (vec- (vec3 1) (vec3 1) (vec3 1)) (vec3 -1))
    (check equal? (vec- (vec3 1) (vec3 1) (vec3 1) (vec3 1)) (vec3 -2)))

  (test-case "vec*"
    (check equal? (vec* (vec3 0)) (vec3 0))
    (check equal? (vec* (vec3 1)) (vec3 1))
    (check equal? (vec* (vec3 2)) (vec3 2))
    (check equal? (vec* (vec3 2) (vec3 3)) (vec3 6))
    (check equal? (vec* (vec3 2) (vec3 3) (vec3 4)) (vec3 24))
    (check equal? (vec* (vec3 1 2 3) (vec3 2 3 4) (vec3 3 4 5)) (vec3 6 24 60)))

  (test-case "vec/"
    (check equal? (vec/ (vec3 1)) (vec3 1))
    (check equal? (vec/ (vec3 0) (vec3 1)) (vec3 0))
    (check equal? (vec/ (vec3 1) (vec3 2)) (vec3 1/2))
    (check equal? (vec/ (vec3 1) (vec3 2) (vec3 2)) (vec3 1/4))
    (check equal? (vec/ (vec3 1) (vec3 2) (vec3 2) (vec3 2)) (vec3 1/8)))

  ;;; ..........................................................................

  (test-case "equal?"
    (check equal? (vec4 0) (vec4 0))
    (check equal? (vec4 1) (vec4 1))
    (check (negate equal?) (vec4 1) (vec4 0))
    (check (negate equal?) (vec4 0) (vec4 1)))

  (test-case "vec+="
    (define v
      (vec4 (vec4 0))) (check equal? v (vec4 0))
    (vec+= v (vec4 1)) (check equal? v (vec4 1))
    (vec+= v (vec4 1)) (check equal? v (vec4 2)))

  (test-case "vec-="
    (define v
      (vec4 (vec4 1))) (check equal? v (vec4 1))
    (vec-= v (vec4 1)) (check equal? v (vec4 0))
    (vec-= v (vec4 1)) (check equal? v (vec4 -1)))

  (test-case "vec*="
    (define v
      (vec4 (vec4 1))) (check equal? v (vec4 1))
    (vec*= v (vec4 1)) (check equal? v (vec4 1))
    (vec*= v (vec4 2)) (check equal? v (vec4 2))
    (vec*= v (vec4 2)) (check equal? v (vec4 4)))

  (test-case "vec/="
    (define v
      (vec4 (vec4 1))) (check equal? v (vec4 1.0))
    (vec/= v (vec4 2)) (check equal? v (vec4 0.5))
    (vec/= v (vec4 5)) (check equal? v (vec4 0.1)))

  (test-case "vec+"
    (check equal? (vec+ (vec4 1)) (vec4 1))
    (check equal? (vec+ (vec4 1) (vec4 1)) (vec4 2))
    (check equal? (vec+ (vec4 1) (vec4 1) (vec4 1)) (vec4 3)))

  (test-case "vec-"
    (check equal? (vec- (vec4 0)) (vec4 0))
    (check equal? (vec- (vec4 1) (vec4 0)) (vec4 1))
    (check equal? (vec- (vec4 1)) (vec4 -1))
    (check equal? (vec- (vec4 1) (vec4 1)) (vec4 0))
    (check equal? (vec- (vec4 1) (vec4 1) (vec4 1)) (vec4 -1))
    (check equal? (vec- (vec4 1) (vec4 1) (vec4 1) (vec4 1)) (vec4 -2)))

  (test-case "vec*"
    (check equal? (vec* (vec4 0)) (vec4 0))
    (check equal? (vec* (vec4 1)) (vec4 1))
    (check equal? (vec* (vec4 2)) (vec4 2))
    (check equal? (vec* (vec4 2) (vec4 3)) (vec4 6))
    (check equal? (vec* (vec4 2) (vec4 3) (vec4 4)) (vec4 24))
    (check equal? (vec* (vec4 1 2 3 4)
                        (vec4 2 3 4 5)
                        (vec4 3 4 5 6)) (vec4 6 24 60 120)))

  (test-case "vec/"
    (check equal? (vec/ (vec4 1)) (vec4 1))
    (check equal? (vec/ (vec4 0) (vec4 1)) (vec4 0))
    (check equal? (vec/ (vec4 1) (vec4 2)) (vec4 1/2))
    (check equal? (vec/ (vec4 1) (vec4 2) (vec4 2)) (vec4 1/4))
    (check equal? (vec/ (vec4 1) (vec4 2) (vec4 2) (vec4 2)) (vec4 1/8)))

  (test-case "vec++"
    (define v
      (vec4 (vec4 0))) (check equal? v (vec4 0))
    (vec++ v) (check equal? v (vec4 1))
    (vec++ v) (check equal? v (vec4 2)))

  (test-case "vec--"
    (define v
      (vec4 (vec4 0))) (check equal? v (vec4 0))
    (vec-- v) (check equal? v (vec4 -1))
    (vec-- v) (check equal? v (vec4 -2)))

  (test-case "vec+"
    (check equal? (vec4 0) (vec+ (vec4 0)))
    (check equal? (vec4 1) (vec+ (vec4 1)))
    (check equal? (vec4 2) (vec+ (vec4 1) 1))
    (check equal? (vec4 2) (vec+ (vec4 1) (vec4 1)))
    (check equal? (vec4 3) (vec+ (vec4 1) 1 (vec4 1)))
    (check equal? (vec4 3) (vec+ (vec4 1) (vec4 2))))

  (test-case "vec-"
    (check equal? (vec4  0) (vec- (vec4 0)))
    (check equal? (vec4 -1) (vec- (vec4 1)))
    (check equal? (vec4  0) (vec- (vec4 1) 1))
    (check equal? (vec4  0) (vec- (vec4 1) (vec4 1)))
    (check equal? (vec4 -1) (vec- (vec4 1) 1 (vec4 1)))
    (check equal? (vec4 -1) (vec- (vec4 1) (vec4 2))))

  (test-case "vec*"
    (check equal? (vec4 0) (vec* (vec4)))
    (check equal? (vec4 0) (vec* (vec4 0) 0))
    (check equal? (vec4 0) (vec* (vec4) (vec4 0)))
    (check equal? (vec4 4) (vec* (vec4 2) 2))
    (check equal? (vec4 6) (vec* (vec4 2) (vec4 3)))
    (check equal? (vec4 6) (vec* (vec4 1) 2 (vec4 3))))

  (test-case "vec/"
    (check equal? (vec4 1) (vec/ (vec4 1)))
    (check equal? (vec4 2) (vec/ (vec4 6) 3))
    (check equal? (vec4 2) (vec/ (vec4 6) (vec4 3)))
    (check equal? (vec4 1) (vec/ (vec4 6) 3 (vec4 2)))))
