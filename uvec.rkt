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

(define (_uvec len) (_array _uint len))

(define/contract (uvec-scalar a)
  (-> integer? exact-nonnegative-integer?)
  (if (inexact? a) (inexact->exact a) a))

(struct uvec (data length)
  #:transparent
  #:name glm:uvec
  #:constructor-name make-uvec
  #:methods gen:equal+hash
  [(define/generic gen-hash-proc hash-proc)
   (define/generic gen-hash2-proc hash2-proc)
   (define (equal-proc v1 v2 _)
     (and (= (uvec-length v1) (uvec-length v2))
          (for/and ([x1 (in-uvec v1)]
                    [x2 (in-uvec v2)])
            (= x1 x2))))
   (define (hash-proc v _) (gen-hash-proc (uvec-data v)))
   (define (hash2-proc v _) (gen-hash2-proc (uvec-data v)))]
  #:methods gen:custom-write
  [(define (write-proc v port mode)
     (case mode
       [(#t #f) (fprintf port "#<uvec~a>" (uvec-length v))]
       [(1 0)
        (define printer (if (pretty-printing) pretty-print print))
        (printer `(,@(if (<= (uvec-length v) 4)
                         (list (uvec-name v))
                         `(uvec #:length ,(uvec-length v)))
                   ,@(uvec->list v))
                 port mode)]))])

(define/contract (uvec #:length [len #f] . as)
  (->* () (#:length (or/c exact-nonnegative-integer? #f))
       #:rest (listof (or/c uvec? (and/c integer? (not/c negative?))))
       uvec?)
  (define xs
    (apply append (for/list ([a (in-list as)])
                    (if (uvec? a)
                        (apply to-uvec-scalars (uvec->list a))
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
  (define data (apply make-uvec-data xs))
  (make-uvec data (array-length data)))

(define to-uvec-scalars
  (case-lambda
    [() null]
    [(a . as) (if (uvec? a)
                  (append (uvec->list a) (apply to-uvec-scalars as))
                  (cons a (apply to-uvec-scalars as)))]))

(define/contract (make-uvec-data . args)
  (->* (number?) #:rest (listof (and/c integer? (not/c negative?))) array?)
  (define len (length args))
  (define data (ptr-ref (malloc (_uvec len) 'atomic) (_uvec len) 0))
  (for ([k (in-range len)]
        [x (in-list (map uvec-scalar args))])
    (array-set! data k x))
  data)

(define/contract (uvec-copy v)
  (-> uvec? uvec?)
  (apply uvec (uvec->list v)))

(define/contract (uvec-name v)
  (-> uvec? symbol?)
  (string->symbol (format "uvec~a" (uvec-length v))))

(define (uvec-ref v i)
  (array-ref (uvec-data v) i))

(define (uvec-set! v i x)
  (array-set! (uvec-data v) i (uvec-scalar x)))

(define/contract (uvec->list v)
  (-> uvec? (listof exact-nonnegative-integer?))
  (sequence->list (in-array (uvec-data v))))

(define/contract (uvec->u32vector v)
  (-> uvec? u32vector?)
  (apply u32vector (uvec->list v)))

(define/contract (in-uvec v)
  (-> uvec? sequence?)
  (in-array (uvec-data v)))

(define/contract (uvec-constructor v)
  (-> uvec? (unconstrained-domain-> uvec?))
  (curry uvec #:length (uvec-length v)))

(define/contract (uvec=! v u)
  (-> uvec? uvec? void?)
  (for ([i (in-range (uvec-length v))]
        [ui (in-uvec u)])
    (uvec-set! v i ui)))

(define/contract ((make-uvec-binop op x0) . args)
  (-> (-> number? number? number?)
      number?
      (unconstrained-domain-> (or/c uvec? number?)))
  (define (uvec-uvec v1 v2)
    (apply (uvec-constructor v1)
           (for/list ([x1 (in-uvec v1)]
                      [x2 (in-uvec v2)])
             (op x1 x2))))
  (define uvec-binop
    (case-lambda
      [(a) (if (uvec? a)
               (uvec-binop ((uvec-constructor a) (uvec-scalar x0)) a)
               (op x0 a))]
      [(a b) (cond [(and (uvec? a) (uvec? b)) (uvec-uvec a b)]
                   [(uvec? a) (uvec-uvec a ((uvec-constructor a) b))]
                   [(uvec? b) (uvec-uvec ((uvec-constructor b) a) b)]
                   [else (op a b)])]
      [(a b . cs) (apply uvec-binop (uvec-binop a b) cs)]))
  (apply uvec-binop args))

(define uvec+ (make-uvec-binop + 0))
(define uvec- (make-uvec-binop - 0))
(define uvec* (make-uvec-binop * 1))
(define uvec/ (make-uvec-binop / 1))

(define (uvec+= v a) (uvec=! v (uvec+ v a)))
(define (uvec-= v a) (uvec=! v (uvec- v a)))
(define (uvec*= v a) (uvec=! v (uvec* v a)))
(define (uvec/= v a) (uvec=! v (uvec/ v a)))

(define (++uvec v) (uvec+= v ((uvec-constructor v) 1.0)) v)
(define (--uvec v) (uvec-= v ((uvec-constructor v) 1.0)) v)
(define (uvec++ v) (begin0 (uvec-copy v) (++uvec v)))
(define (uvec-- v) (begin0 (uvec-copy v) (--uvec v)))

;;; ----------------------------------------------------------------------------

(define-syntax (define-uvec stx)
  (syntax-case stx ()
    [(_ type-id len)
     (identifier? #'type-id)
     (with-syntax ([type-id? (format-id #'type-id "~a?" #'type-id)])
       #'(begin
           (define/contract (type-id? a)
             predicate/c
             (and (uvec? a) (= (uvec-length a) len)))
           (define type-id (curry uvec #:length len))))]))

(define-uvec uvec1 1)
(define-uvec uvec2 2)
(define-uvec uvec3 3)
(define-uvec uvec4 4)

;;; ============================================================================

(module+ test
  (require racket/function
           rackunit)

  (test-case "equal?"
    (check equal? (uvec2 0) (uvec2 0))
    (check equal? (uvec2 1) (uvec2 1))
    (check (negate equal?) (uvec2 1) (uvec2 0))
    (check (negate equal?) (uvec2 0) (uvec2 1)))

  (test-case "uvec+="
    (define v
      (uvec2 (uvec2 0))) (check equal? v (uvec2 0))
    (uvec+= v (uvec2 1)) (check equal? v (uvec2 1))
    (uvec+= v (uvec2 1)) (check equal? v (uvec2 2)))

  (test-case "uvec-="
    (define v
      (uvec2 (uvec2 1))) (check equal? v (uvec2 1))
    (uvec-= v (uvec2 1)) (check equal? v (uvec2 0))
    (check-exn exn:fail? (λ () (uvec-= v (uvec2 1)))))

  (test-case "uvec*="
    (define v
      (uvec2 (uvec2 1))) (check equal? v (uvec2 1))
    (uvec*= v (uvec2 1)) (check equal? v (uvec2 1))
    (uvec*= v (uvec2 2)) (check equal? v (uvec2 2))
    (uvec*= v (uvec2 2)) (check equal? v (uvec2 4)))

  (test-case "uvec/="
    (define v
      (uvec2 (uvec2 6))) (check equal? v (uvec2 6))
    (uvec/= v (uvec2 3)) (check equal? v (uvec2 2))
    (uvec/= v (uvec2 2)) (check equal? v (uvec2 1))
    (check-exn exn:fail:contract? (λ () (uvec/= v (uvec2 2)))))

  (test-case "uvec+"
    (check equal? (uvec+ (uvec2 1)) (uvec2 1))
    (check equal? (uvec+ (uvec2 1) (uvec2 1)) (uvec2 2))
    (check equal? (uvec+ (uvec2 1) (uvec2 1) (uvec2 1)) (uvec2 3)))

  (test-case "uvec-"
    (check equal? (uvec- (uvec2 0)) (uvec2 0))
    (check equal? (uvec- (uvec2 1) (uvec2 0)) (uvec2 1))
    (check equal? (uvec- (uvec2 1) (uvec2 1)) (uvec2 0))
    (check-exn exn:fail:contract? (λ () (uvec- (uvec2 1)))))

  (test-case "uvec*"
    (check equal? (uvec* (uvec2 0)) (uvec2 0))
    (check equal? (uvec* (uvec2 1)) (uvec2 1))
    (check equal? (uvec* (uvec2 2)) (uvec2 2))
    (check equal? (uvec* (uvec2 2) (uvec2 3)) (uvec2 6))
    (check equal? (uvec* (uvec2 2) (uvec2 3) (uvec2 4)) (uvec2 24))
    (check equal? (uvec* (uvec2 1 2) (uvec2 2 3) (uvec2 3 4)) (uvec2 6 24))
    (check-exn exn:fail:contract? (λ () (uvec* (uvec2 1) -1))))

  (test-case "uvec/"
    (check equal? (uvec/ (uvec2 1)) (uvec2 1))
    (check equal? (uvec/ (uvec2 0) (uvec2 1)) (uvec2 0))
    (check equal? (uvec/ (uvec2 2) (uvec2 1)) (uvec2 2))
    (check equal? (uvec/ (uvec2 6) (uvec2 2) (uvec2 1)) (uvec2 3))
    (check-exn exn:fail:contract? (λ () (uvec/ (uvec2 1) (uvec2 2)))))

  ;;; ..........................................................................

  (test-case "equal?"
    (check equal? (uvec3 0) (uvec3 0))
    (check equal? (uvec3 1) (uvec3 1))
    (check (negate equal?) (uvec3 1) (uvec3 0))
    (check (negate equal?) (uvec3 0) (uvec3 1)))

  (test-case "uvec+="
    (define v
      (uvec3 (uvec3 0))) (check equal? v (uvec3 0))
    (uvec+= v (uvec3 1)) (check equal? v (uvec3 1))
    (uvec+= v (uvec3 1)) (check equal? v (uvec3 2)))

  (test-case "uvec-="
    (define v
      (uvec3 (uvec3 1))) (check equal? v (uvec3 1))
    (uvec-= v (uvec3 1)) (check equal? v (uvec3 0))
    (check-exn exn:fail? (λ () (uvec-= v (uvec3 1)))))

  (test-case "uvec*="
    (define v
      (uvec3 (uvec3 1))) (check equal? v (uvec3 1))
    (uvec*= v (uvec3 1)) (check equal? v (uvec3 1))
    (uvec*= v (uvec3 2)) (check equal? v (uvec3 2))
    (uvec*= v (uvec3 2)) (check equal? v (uvec3 4)))

  (test-case "uvec/="
    (define v
      (uvec3 (uvec3 6))) (check equal? v (uvec3 6))
    (uvec/= v (uvec3 3)) (check equal? v (uvec3 2))
    (uvec/= v (uvec3 2)) (check equal? v (uvec3 1))
    (check-exn exn:fail:contract? (λ () (uvec/= v (uvec3 2)))))

  (test-case "uvec+"
    (check equal? (uvec+ (uvec3 1)) (uvec3 1))
    (check equal? (uvec+ (uvec3 1) (uvec3 1)) (uvec3 2))
    (check equal? (uvec+ (uvec3 1) (uvec3 1) (uvec3 1)) (uvec3 3)))

  (test-case "uvec-"
    (check equal? (uvec- (uvec3 0)) (uvec3 0))
    (check equal? (uvec- (uvec3 1) (uvec3 0)) (uvec3 1))
    (check equal? (uvec- (uvec3 1) (uvec3 1)) (uvec3 0))
    (check-exn exn:fail:contract? (λ () (uvec- (uvec3 1)))))

  (test-case "uvec*"
    (check equal? (uvec* (uvec3 0)) (uvec3 0))
    (check equal? (uvec* (uvec3 1)) (uvec3 1))
    (check equal? (uvec* (uvec3 2)) (uvec3 2))
    (check equal? (uvec* (uvec3 2) (uvec3 3)) (uvec3 6))
    (check equal? (uvec* (uvec3 2) (uvec3 3) (uvec3 4)) (uvec3 24))
    (check equal? (uvec* (uvec3 1 2 3) (uvec3 2 3 4) (uvec3 3 4 5)) (uvec3 6 24 60))
    (check-exn exn:fail:contract? (λ () (uvec* (uvec3 1) -1))))

  (test-case "uvec/"
    (check equal? (uvec/ (uvec3 1)) (uvec3 1))
    (check equal? (uvec/ (uvec3 0) (uvec3 1)) (uvec3 0))
    (check equal? (uvec/ (uvec3 2) (uvec3 1)) (uvec3 2))
    (check equal? (uvec/ (uvec3 6) (uvec3 2) (uvec3 1)) (uvec3 3))
    (check-exn exn:fail:contract? (λ () (uvec/ (uvec3 1) (uvec3 2)))))

  ;;; ..........................................................................

  (test-case "equal?"
    (check equal? (uvec4 0) (uvec4 0))
    (check equal? (uvec4 1) (uvec4 1))
    (check (negate equal?) (uvec4 1) (uvec4 0))
    (check (negate equal?) (uvec4 0) (uvec4 1)))

  (test-case "uvec+="
    (define v
      (uvec4 (uvec4 0))) (check equal? v (uvec4 0))
    (uvec+= v (uvec4 1)) (check equal? v (uvec4 1))
    (uvec+= v (uvec4 1)) (check equal? v (uvec4 2)))

  (test-case "uvec-="
    (define v
      (uvec4 (uvec4 1))) (check equal? v (uvec4 1))
    (uvec-= v (uvec4 1)) (check equal? v (uvec4 0))
    (check-exn exn:fail? (λ () (uvec-= v (uvec4 1)))))

  (test-case "uvec*="
    (define v
      (uvec4 (uvec4 1))) (check equal? v (uvec4 1))
    (uvec*= v (uvec4 1)) (check equal? v (uvec4 1))
    (uvec*= v (uvec4 2)) (check equal? v (uvec4 2))
    (uvec*= v (uvec4 2)) (check equal? v (uvec4 4)))

  (test-case "uvec/="
    (define v
      (uvec4 (uvec4 6))) (check equal? v (uvec4 6))
    (uvec/= v (uvec4 3)) (check equal? v (uvec4 2))
    (uvec/= v (uvec4 2)) (check equal? v (uvec4 1))
    (check-exn exn:fail:contract? (λ () (uvec/= v (uvec4 2)))))

  (test-case "uvec+"
    (check equal? (uvec+ (uvec4 1)) (uvec4 1))
    (check equal? (uvec+ (uvec4 1) (uvec4 1)) (uvec4 2))
    (check equal? (uvec+ (uvec4 1) (uvec4 1) (uvec4 1)) (uvec4 3)))

  (test-case "uvec-"
    (check equal? (uvec- (uvec4 0)) (uvec4 0))
    (check equal? (uvec- (uvec4 1) (uvec4 0)) (uvec4 1))
    (check equal? (uvec- (uvec4 1) (uvec4 1)) (uvec4 0))
    (check-exn exn:fail:contract? (λ () (uvec- (uvec4 1)))))

  (test-case "uvec*"
    (check equal? (uvec* (uvec4 0)) (uvec4 0))
    (check equal? (uvec* (uvec4 1)) (uvec4 1))
    (check equal? (uvec* (uvec4 2)) (uvec4 2))
    (check equal? (uvec* (uvec4 2) (uvec4 3)) (uvec4 6))
    (check equal? (uvec* (uvec4 2) (uvec4 3) (uvec4 4)) (uvec4 24))
    (check equal? (uvec* (uvec4 1 2 3 4) (uvec4 2 3 4 5) (uvec4 3 4 5 6)) (uvec4 6 24 60 120))
    (check-exn exn:fail:contract? (λ () (uvec* (uvec4 1) -1))))

  (test-case "uvec/"
    (check equal? (uvec/ (uvec4 1)) (uvec4 1))
    (check equal? (uvec/ (uvec4 0) (uvec4 1)) (uvec4 0))
    (check equal? (uvec/ (uvec4 2) (uvec4 1)) (uvec4 2))
    (check equal? (uvec/ (uvec4 6) (uvec4 2) (uvec4 1)) (uvec4 3))
    (check-exn exn:fail:contract? (λ () (uvec/ (uvec4 1) (uvec4 2)))))

  (test-case "uvec++"
    (define v (uvec4 (uvec4 0))) (check equal? v (uvec4 0))
    (uvec++ v) (check equal? v (uvec4 1))
    (uvec++ v) (check equal? v (uvec4 2)))

  (test-case "uvec--"
    (define v (uvec4 (uvec4 2))) (check equal? v (uvec4 2))
    (uvec-- v) (check equal? v (uvec4 1))
    (uvec-- v) (check equal? v (uvec4 0))
    (check-exn exn:fail:contract? (λ () (uvec-- v)))))
