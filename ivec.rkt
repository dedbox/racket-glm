#lang racket/base

(require (except-in ffi/unsafe ->)
         ffi/vector
         racket/contract
         racket/function
         racket/generic
         racket/list
         racket/pretty
         racket/sequence
         (for-syntax racket/base
                     racket/syntax))

(provide (all-defined-out))

(define (_ivec len) (_array _int len))

(define/contract (ivec-scalar a)
  (-> integer? exact-integer?)
  (if (inexact? a) (inexact->exact a) a))

(struct ivec (data length)
  #:transparent
  #:name glm:ivec
  #:constructor-name make-ivec
  #:methods gen:equal+hash
  [(define/generic gen-hash-proc hash-proc)
   (define/generic gen-hash2-proc hash2-proc)
   (define (equal-proc v1 v2 _)
     (and (= (ivec-length v1) (ivec-length v2))
          (for/and ([x1 (in-ivec v1)]
                    [x2 (in-ivec v2)])
            (= x1 x2))))
   (define (hash-proc v _) (gen-hash-proc (ivec-data v)))
   (define (hash2-proc v _) (gen-hash2-proc (ivec-data v)))]
  #:methods gen:custom-write
  [(define (write-proc v port mode)
     (case mode
       [(#t #f) (fprintf port "#<ivec~a>" (ivec-length v))]
       [(1 0)
        (define printer (if (pretty-printing) pretty-print print))
        (printer `(,@(if (<= (ivec-length v) 4)
                         (list (ivec-name v))
                         `(ivec #:length ,(ivec-length v)))
                   ,@(ivec->list v))
                 port mode)]))])

(define/contract (ivec #:length [len #f] . as)
  (->* () (#:length (or/c exact-nonnegative-integer? #f))
       #:rest (listof (or/c ivec? integer?))
       ivec?)
  (define xs
    (apply append (for/list ([a (in-list as)])
                    (if (ivec? a)
                        (apply to-ivec-scalars (ivec->list a))
                        (list a)))))
  (when len
    (define xs-len (length xs))
    (set! xs
      (cond
        [(= xs-len 0) (build-list len (λ _ 0))]
        [(= xs-len 1) (build-list len (λ _ (car xs)))]
        [(= xs-len len) xs]
        [(> xs-len len) (take xs len)]
        [(< xs-len len) (append xs (build-list (- len (length xs)) (λ _ 0)))])))
  (define data (apply make-ivec-data xs))
  (make-ivec data (array-length data)))

(define to-ivec-scalars
  (case-lambda
    [() null]
    [(a . as) (if (ivec? a)
                  (append (ivec->list a) (apply to-ivec-scalars as))
                  (cons a (apply to-ivec-scalars as)))]))

(define/contract (make-ivec-data . args)
  (->* (number?) #:rest (listof integer?) array?)
  (define len (length args))
  (define data (ptr-ref (malloc (_ivec len) 'atomic) (_ivec len) 0))
  (for ([k (in-range len)]
        [x (in-list (map ivec-scalar args))])
    (array-set! data k x))
  data)

(define/contract (ivec-copy v)
  (-> ivec? ivec?)
  (apply ivec (ivec->list v)))

(define/contract (ivec-name v)
  (-> ivec? symbol?)
  (string->symbol (format "ivec~a" (ivec-length v))))

(define (ivec-ref v i)
  (array-ref (ivec-data v) i))

(define (ivec-set! v i x)
  (array-set! (ivec-data v) i (ivec-scalar x)))

(define/contract (ivec->list v)
  (-> ivec? (listof exact-integer?))
  (sequence->list (in-array (ivec-data v))))

(define/contract (ivec->u32vector v)
  (-> ivec? u32vector?)
  (apply u32vector (ivec->list v)))

(define/contract (in-ivec v)
  (-> ivec? sequence?)
  (in-array (ivec-data v)))

(define/contract (ivec-constructor v)
  (-> ivec? (unconstrained-domain-> ivec?))
  (curry ivec #:length (ivec-length v)))

(define/contract (ivec=! v u)
  (-> ivec? ivec? void?)
  (for ([i (in-range (ivec-length v))]
        [ui (in-ivec u)])
    (ivec-set! v i ui)))

(define/contract ((make-ivec-binop op x0) . args)
  (-> (-> number? number? number?)
      number?
      (unconstrained-domain-> (or/c ivec? number?)))
  (define (ivec-ivec v1 v2)
    (apply (ivec-constructor v1)
           (for/list ([x1 (in-ivec v1)]
                      [x2 (in-ivec v2)])
             (op x1 x2))))
  (define ivec-binop
    (case-lambda
      [(a) (if (ivec? a)
               (ivec-binop ((ivec-constructor a) (ivec-scalar x0)) a)
               (op x0 a))]
      [(a b) (cond [(and (ivec? a) (ivec? b)) (ivec-ivec a b)]
                   [(ivec? a) (ivec-ivec a ((ivec-constructor a) b))]
                   [(ivec? b) (ivec-ivec ((ivec-constructor b) a) b)]
                   [else (op a b)])]
      [(a b . cs) (apply ivec-binop (ivec-binop a b) cs)]))
  (apply ivec-binop args))

(define ivec+ (make-ivec-binop + 0))
(define ivec- (make-ivec-binop - 0))
(define ivec* (make-ivec-binop * 1))
(define ivec/ (make-ivec-binop / 1))

(define (ivec+= v a) (ivec=! v (ivec+ v a)))
(define (ivec-= v a) (ivec=! v (ivec- v a)))
(define (ivec*= v a) (ivec=! v (ivec* v a)))
(define (ivec/= v a) (ivec=! v (ivec/ v a)))

(define (++ivec v) (ivec+= v ((ivec-constructor v) 1.0)) v)
(define (--ivec v) (ivec-= v ((ivec-constructor v) 1.0)) v)
(define (ivec++ v) (begin0 (ivec-copy v) (++ivec v)))
(define (ivec-- v) (begin0 (ivec-copy v) (--ivec v)))

;;; ----------------------------------------------------------------------------

(define-syntax (define-ivec stx)
  (syntax-case stx ()
    [(_ type-id len)
     (identifier? #'type-id)
     (with-syntax ([type-id? (format-id #'type-id "~a?" #'type-id)])
       #'(begin
           (define/contract (type-id? a)
             predicate/c
             (and (ivec? a) (= (ivec-length a) len)))
           (define type-id (curry ivec #:length len))))]))

(define-ivec ivec1 1)
(define-ivec ivec2 2)
(define-ivec ivec3 3)
(define-ivec ivec4 4)

;;; ============================================================================

(module+ test
  (require racket/function
           rackunit)

  (test-case "equal?"
    (check equal? (ivec2 0) (ivec2 0))
    (check equal? (ivec2 1) (ivec2 1))
    (check (negate equal?) (ivec2 1) (ivec2 0))
    (check (negate equal?) (ivec2 0) (ivec2 1)))

  (test-case "ivec+="
    (define v
      (ivec2 (ivec2 0))) (check equal? v (ivec2 0))
    (ivec+= v (ivec2 1)) (check equal? v (ivec2 1))
    (ivec+= v (ivec2 1)) (check equal? v (ivec2 2)))

  (test-case "ivec-="
    (define v
      (ivec2 (ivec2 1))) (check equal? v (ivec2  1))
    (ivec-= v (ivec2 1)) (check equal? v (ivec2  0))
    (ivec-= v (ivec2 1)) (check equal? v (ivec2 -1)))

  (test-case "ivec*="
    (define v
      (ivec2 (ivec2 1))) (check equal? v (ivec2 1))
    (ivec*= v (ivec2 1)) (check equal? v (ivec2 1))
    (ivec*= v (ivec2 2)) (check equal? v (ivec2 2))
    (ivec*= v (ivec2 2)) (check equal? v (ivec2 4)))

  (test-case "ivec/="
    (define v
      (ivec2 (ivec2 6))) (check equal? v (ivec2 6))
    (ivec/= v (ivec2 3)) (check equal? v (ivec2 2))
    (ivec/= v (ivec2 2)) (check equal? v (ivec2 1))
    (check-exn exn:fail:contract? (λ () (ivec/= v (ivec2 2)))))

  (test-case "ivec+"
    (check equal? (ivec+ (ivec2 1)) (ivec2 1))
    (check equal? (ivec+ (ivec2 1) (ivec2 1)) (ivec2 2))
    (check equal? (ivec+ (ivec2 1) (ivec2 1) (ivec2 1)) (ivec2 3)))

  (test-case "ivec-"
    (check equal? (ivec- (ivec2 0)) (ivec2 0))
    (check equal? (ivec- (ivec2 1) (ivec2 0)) (ivec2  1))
    (check equal? (ivec- (ivec2 1) (ivec2 1)) (ivec2  0))
    (check equal? (ivec- (ivec2 1) (ivec2 1) (ivec2 1)) (ivec2 -1)))

  (test-case "ivec*"
    (check equal? (ivec* (ivec2 0)) (ivec2 0))
    (check equal? (ivec* (ivec2 1)) (ivec2 1))
    (check equal? (ivec* (ivec2 2)) (ivec2 2))
    (check equal? (ivec* (ivec2 2) (ivec2 3)) (ivec2 6))
    (check equal? (ivec* (ivec2 2) (ivec2 3) (ivec2 4)) (ivec2 24))
    (check equal? (ivec* (ivec2 1 2) (ivec2 2 3) (ivec2 3 4)) (ivec2 6 24))
    (check equal? (ivec* (ivec2 1) -1) (ivec2 -1)))

  (test-case "ivec/"
    (check equal? (ivec/ (ivec2 1)) (ivec2 1))
    (check equal? (ivec/ (ivec2 0) (ivec2 1)) (ivec2 0))
    (check equal? (ivec/ (ivec2 2) (ivec2 1)) (ivec2 2))
    (check equal? (ivec/ (ivec2 6) (ivec2 2) (ivec2 1)) (ivec2 3))
    (check-exn exn:fail:contract? (λ () (ivec/ (ivec2 1) (ivec2 2)))))

  ;;; ..........................................................................

  (test-case "equal?"
    (check equal? (ivec3 0) (ivec3 0))
    (check equal? (ivec3 1) (ivec3 1))
    (check (negate equal?) (ivec3 1) (ivec3 0))
    (check (negate equal?) (ivec3 0) (ivec3 1)))

  (test-case "ivec+="
    (define v
      (ivec3 (ivec3 0))) (check equal? v (ivec3 0))
    (ivec+= v (ivec3 1)) (check equal? v (ivec3 1))
    (ivec+= v (ivec3 1)) (check equal? v (ivec3 2)))

  (test-case "ivec-="
    (define v
      (ivec3 (ivec3 1))) (check equal? v (ivec3  1))
    (ivec-= v (ivec3 1)) (check equal? v (ivec3  0))
    (ivec-= v (ivec3 1)) (check equal? v (ivec3 -1)))

  (test-case "ivec*="
    (define v
      (ivec3 (ivec3 1))) (check equal? v (ivec3 1))
    (ivec*= v (ivec3 1)) (check equal? v (ivec3 1))
    (ivec*= v (ivec3 2)) (check equal? v (ivec3 2))
    (ivec*= v (ivec3 2)) (check equal? v (ivec3 4)))

  (test-case "ivec/="
    (define v
      (ivec3 (ivec3 6))) (check equal? v (ivec3 6))
    (ivec/= v (ivec3 3)) (check equal? v (ivec3 2))
    (ivec/= v (ivec3 2)) (check equal? v (ivec3 1))
    (check-exn exn:fail:contract? (λ () (ivec/= v (ivec3 2)))))

  (test-case "ivec+"
    (check equal? (ivec+ (ivec3 1)) (ivec3 1))
    (check equal? (ivec+ (ivec3 1) (ivec3 1)) (ivec3 2))
    (check equal? (ivec+ (ivec3 1) (ivec3 1) (ivec3 1)) (ivec3 3)))

  (test-case "ivec-"
    (check equal? (ivec- (ivec3 0)) (ivec3 0))
    (check equal? (ivec- (ivec3 1) (ivec3 0)) (ivec3  1))
    (check equal? (ivec- (ivec3 1) (ivec3 1)) (ivec3  0))
    (check equal? (ivec- (ivec3 1) (ivec3 1) (ivec3 1)) (ivec3 -1)))

  (test-case "ivec*"
    (check equal? (ivec* (ivec3 0)) (ivec3 0))
    (check equal? (ivec* (ivec3 1)) (ivec3 1))
    (check equal? (ivec* (ivec3 2)) (ivec3 2))
    (check equal? (ivec* (ivec3 2) (ivec3 3)) (ivec3 6))
    (check equal? (ivec* (ivec3 2) (ivec3 3) (ivec3 4)) (ivec3 24))
    (check equal? (ivec* (ivec3 1 2 3) (ivec3 2 3 4) (ivec3 3 4 5)) (ivec3 6 24 60))
    (check equal? (ivec* (ivec3 1) -1) (ivec3 -1)))

  (test-case "ivec/"
    (check equal? (ivec/ (ivec3 1)) (ivec3 1))
    (check equal? (ivec/ (ivec3 0) (ivec3 1)) (ivec3 0))
    (check equal? (ivec/ (ivec3 2) (ivec3 1)) (ivec3 2))
    (check equal? (ivec/ (ivec3 6) (ivec3 2) (ivec3 1)) (ivec3 3))
    (check-exn exn:fail:contract? (λ () (ivec/ (ivec3 1) (ivec3 2)))))

  ;;; ..........................................................................

  (test-case "equal?"
    (check equal? (ivec4 0) (ivec4 0))
    (check equal? (ivec4 1) (ivec4 1))
    (check (negate equal?) (ivec4 1) (ivec4 0))
    (check (negate equal?) (ivec4 0) (ivec4 1)))

  (test-case "ivec+="
    (define v
      (ivec4 (ivec4 0))) (check equal? v (ivec4 0))
    (ivec+= v (ivec4 1)) (check equal? v (ivec4 1))
    (ivec+= v (ivec4 1)) (check equal? v (ivec4 2)))

  (test-case "ivec-="
    (define v
      (ivec4 (ivec4 1))) (check equal? v (ivec4  1))
    (ivec-= v (ivec4 1)) (check equal? v (ivec4  0))
    (ivec-= v (ivec4 1)) (check equal? v (ivec4 -1)))

  (test-case "ivec*="
    (define v
      (ivec4 (ivec4 1))) (check equal? v (ivec4 1))
    (ivec*= v (ivec4 1)) (check equal? v (ivec4 1))
    (ivec*= v (ivec4 2)) (check equal? v (ivec4 2))
    (ivec*= v (ivec4 2)) (check equal? v (ivec4 4)))

  (test-case "ivec/="
    (define v
      (ivec4 (ivec4 6))) (check equal? v (ivec4 6))
    (ivec/= v (ivec4 3)) (check equal? v (ivec4 2))
    (ivec/= v (ivec4 2)) (check equal? v (ivec4 1))
    (check-exn exn:fail:contract? (λ () (ivec/= v (ivec4 2)))))

  (test-case "ivec+"
    (check equal? (ivec+ (ivec4 1)) (ivec4 1))
    (check equal? (ivec+ (ivec4 1) (ivec4 1)) (ivec4 2))
    (check equal? (ivec+ (ivec4 1) (ivec4 1) (ivec4 1)) (ivec4 3)))

  (test-case "ivec-"
    (check equal? (ivec- (ivec4 0)) (ivec4 0))
    (check equal? (ivec- (ivec4 1) (ivec4 0)) (ivec4 1))
    (check equal? (ivec- (ivec4 1) (ivec4 1)) (ivec4 0))
    (check equal? (ivec- (ivec4 1) (ivec4 1) (ivec4 1)) (ivec4 -1)))

  (test-case "ivec*"
    (check equal? (ivec* (ivec4 0)) (ivec4 0))
    (check equal? (ivec* (ivec4 1)) (ivec4 1))
    (check equal? (ivec* (ivec4 2)) (ivec4 2))
    (check equal? (ivec* (ivec4 2) (ivec4 3)) (ivec4 6))
    (check equal? (ivec* (ivec4 2) (ivec4 3) (ivec4 4)) (ivec4 24))
    (check equal? (ivec* (ivec4 1 2 3 4) (ivec4 2 3 4 5) (ivec4 3 4 5 6)) (ivec4 6 24 60 120))
    (check equal? (ivec* (ivec4 1) -1) (ivec4 -1)))

  (test-case "ivec/"
    (check equal? (ivec/ (ivec4 1)) (ivec4 1))
    (check equal? (ivec/ (ivec4 0) (ivec4 1)) (ivec4 0))
    (check equal? (ivec/ (ivec4 2) (ivec4 1)) (ivec4 2))
    (check equal? (ivec/ (ivec4 6) (ivec4 2) (ivec4 1)) (ivec4 3))
    (check-exn exn:fail:contract? (λ () (ivec/ (ivec4 1) (ivec4 2)))))

  (test-case "ivec++"
    (define v (ivec4 (ivec4 -1))) (check equal? v (ivec4 -1))
    (ivec++ v) (check equal? v (ivec4 0))
    (ivec++ v) (check equal? v (ivec4 1))
    (ivec++ v) (check equal? v (ivec4 2)))

  (test-case "ivec--"
    (define v (ivec4 (ivec4 2))) (check equal? v (ivec4 2))
    (ivec-- v) (check equal? v (ivec4  1))
    (ivec-- v) (check equal? v (ivec4  0))
    (ivec-- v) (check equal? v (ivec4 -1))))
