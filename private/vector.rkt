#lang racket/base

(require (except-in ffi/unsafe ->)
         racket/contract
         racket/format
         racket/function
         racket/generic
         racket/list
         racket/match
         racket/pretty
         racket/sequence
         syntax/parse/define
         (for-syntax racket/base
                     racket/syntax))

(provide (all-defined-out))

(define-simple-macro (define-numeric-vector-type
                       *vec:id
                       _*ctype:id
                       *vec-native?:id
                       *vec-scalar?:id
                       #:from-scalar from-scalar:expr
                       #:to-scalar to-scalar:expr
                       #:to-native to-native:expr
                       #:ffi ffi-vector:id)
  #:with *vec-constructor (format-id #'*vec "~a-constructor" #'*vec)
  #:with make-*vec-binop  (format-id #'*vec "make-~a-binop"  #'*vec)
  #:with *vec-copy (format-id #'*vec "~a-copy" #'*vec)
  #:with *vec=!  (format-id #'*vec "~a=!"  #'*vec)
  #:with *vec+   (format-id #'*vec "~a+"   #'*vec)
  #:with *vec-   (format-id #'*vec "~a-"   #'*vec)
  #:with *vec*   (format-id #'*vec "~a*"   #'*vec)
  #:with *vec/   (format-id #'*vec "~a/"   #'*vec)
  #:with *vec+=! (format-id #'*vec "~a+=!" #'*vec)
  #:with *vec-=! (format-id #'*vec "~a-=!" #'*vec)
  #:with *vec*=! (format-id #'*vec "~a*=!" #'*vec)
  #:with *vec/=! (format-id #'*vec "~a/=!" #'*vec)
  #:with ++*vec! (format-id #'*vec "++~a!" #'*vec)
  #:with --*vec! (format-id #'*vec "--~a!" #'*vec)
  #:with *vec++! (format-id #'*vec "~a++!" #'*vec)
  #:with *vec--! (format-id #'*vec "~a--!" #'*vec)
  (begin
    (define-vector-type *vec _*ctype *vec-native? *vec-scalar? =
      #:from-scalar from-scalar
      #:to-scalar to-scalar
      #:to-native to-native
      #:fill 0
      #:ffi ffi-vector)
    (define *vec+ (make-*vec-binop + 0))
    (define *vec- (make-*vec-binop - 0))
    (define *vec* (make-*vec-binop * 1))
    (define *vec/ (make-*vec-binop / 1))
    (define (*vec+=! v a) (*vec=! v (*vec+ v a)))
    (define (*vec-=! v a) (*vec=! v (*vec- v a)))
    (define (*vec*=! v a) (*vec=! v (*vec* v a)))
    (define (*vec/=! v a) (*vec=! v (*vec/ v a)))
    (define (++*vec! v) (*vec+=! v ((*vec-constructor v) 1.0)) v)
    (define (--*vec! v) (*vec-=! v ((*vec-constructor v) 1.0)) v)
    (define (*vec++! v) (begin0 (*vec-copy v) (++*vec! v)))
    (define (*vec--! v) (begin0 (*vec-copy v) (--*vec! v)))))

(define-simple-macro (define-vector-type
                       *vec:id
                       _*ctype:id
                       *vec-native?:id
                       *vec-scalar?:id
                       *vec-scalar=?:id
                       #:from-scalar from-scalar:id
                       #:to-scalar to-scalar:expr
                       #:to-native to-native:expr
                       #:fill *fill:expr
                       #:ffi ffi-vector:id)
  #:with current-*vec-precision (format-id #'*vec "current-~a-precision" #'*vec)
  #:with *vec->ffi-vector (format-id #'*vec "~a->~a" #'*vec #'ffi-vector)
  #:with *vec-constructor (format-id #'*vec "~a-constructor" #'*vec)
  #:with define-*vec-type (format-id #'*vec "define-~a-type" #'*vec)
  #:with make-*vec-binop  (format-id #'*vec "make-~a-binop"  #'*vec)
  #:with *vec-to-scalar   (format-id #'*vec "~a-to-scalar"   #'*vec)
  #:with *vec-to-native   (format-id #'*vec "~a-to-native"   #'*vec)
  #:with make-*vec-data   (format-id #'*vec "make-~a-data"   #'*vec)
  #:with ffi-vector? (format-id #'ffi-vector "~a?" #'ffi-vector)
  #:with _ffi-vector (format-id #'ffi-vector "_~a" #'ffi-vector)
  #:with _*vec       (format-id #'*vec "_~a"       #'*vec)
  #:with *vec?       (format-id #'*vec "~a?"       #'*vec)
  #:with *vec~       (format-id #'*vec "~~~a~~"    #'*vec)
  #:with glm-*vec    (format-id #'*vec "glm:~a"    #'*vec)
  #:with make-*vec   (format-id #'*vec "make-~a"   #'*vec)
  #:with *vec-data   (format-id #'*vec "~a-data"   #'*vec)
  #:with *vec-length (format-id #'*vec "~a-length" #'*vec)
  #:with *vec-fixed? (format-id #'*vec "~a-fixed?" #'*vec)
  #:with *vec-fill   (format-id #'*vec "~a-fill"   #'*vec)
  #:with *vec-copy   (format-id #'*vec "~a-copy"   #'*vec)
  #:with *vec-name   (format-id #'*vec "~a-name"   #'*vec)
  #:with *vec-ref    (format-id #'*vec "~a-ref"    #'*vec)
  #:with *vec-set!   (format-id #'*vec "~a-set!"   #'*vec)
  #:with *vec->list  (format-id #'*vec "~a->list"  #'*vec)
  #:with in-*vec     (format-id #'*vec "in-~a"     #'*vec)
  #:with for/*vec    (format-id #'*vec "for/~a"    #'*vec)
  #:with for*/*vec   (format-id #'*vec "for*/~a"   #'*vec)
  #:with *vec=!      (format-id #'*vec "~a=!"      #'*vec)
  #:with *vec1       (format-id #'*vec "~a1"       #'*vec)
  #:with *vec2       (format-id #'*vec "~a2"       #'*vec)
  #:with *vec3       (format-id #'*vec "~a3"       #'*vec)
  #:with *vec4       (format-id #'*vec "~a4"       #'*vec)

  (...
   (begin

     (define (_*vec len) (_array _*ctype len))

     (define/contract *vec-to-scalar (-> *vec-native? *vec-scalar?) to-scalar)
     (define/contract *vec-to-native (-> any/c *vec-native?) to-native)

     (define *vec-to-scalars
       (case-lambda
         [() null]
         [(a . as) (if (*vec? a)
                       (append (*vec->list a) (apply *vec-to-scalars as))
                       (cons a (apply *vec-to-scalars as)))]))

     (define-match-expander *vec
       (syntax-parser
         [(_ x ... #:rest tail) #'(? *vec? (app *vec->list (list-rest x ... tail)))]
         [(_ x ...) #'(? *vec? (app *vec->list (list x ...)))])
       (make-rename-transformer #'*vec~))

     (define current-*vec-precision (make-parameter 2))

     (struct *vec (data length fixed? fill)
       #:transparent
       #:name glm-*vec
       #:constructor-name make-*vec
       #:methods gen:equal+hash
       [(define/generic gen-hash-proc hash-proc)
        (define/generic gen-hash2-proc hash2-proc)
        (define (equal-proc v1 v2 _)
          (and (= (*vec-length v1) (*vec-length v2))
               (for/and ([x1 (in-*vec v1)]
                         [x2 (in-*vec v2)])
                 (*vec-scalar=? x1 x2))))
        (define (hash-proc v _) (gen-hash-proc (*vec-data v)))
        (define (hash2-proc v _) (gen-hash2-proc (*vec-data v)))]
       #:methods gen:custom-write
       [(define (write-proc v port mode)
          (case mode
            [(#t #f) (if (*vec-fixed? v)
                         (fprintf port "#<~a~a>" '*vec (*vec-length v))
                         (fprintf port "#<~a>" '*vec))]
            [(1 0)
             (match-define (glm-*vec data len fixed? _) v)
             (define printer (if (pretty-printing) pretty-display display))
             (printer `(,@(if fixed?
                              (if (<= len 4)
                                  `(,(*vec-name v))
                                  `(*vec #:length ,len))
                              `(*vec))
                        ,@(for/list ([x (in-*vec v)]
                                     [i (in-naturals)]
                                     #:when (< i len))
                            (if (number? x)
                                (~r #:precision (current-*vec-precision) x)
                                (~a x))))
                      port)]))])

     (define *vec~ 
       (contract
        (->* () (#:length (or/c exact-positive-integer? #f)
                 #:fill (or/c *vec-native? 'no-fill))
             #:rest (listof (or/c *vec? *vec-native?)) *vec?)
        (procedure-rename
         (λ (#:length [len #f] #:fill [fill *fill] . as)
           (define (easy-args->scalars)
             (define xs (args->scalars))
             (cond [(and (>= len 2) (= (length xs) 1)) (build-list len (λ _ (car xs)))]
                   [(= (length xs) len) xs]
                   [(> (length xs) len) (take xs len)]
                   [(< (length xs) len)
                    (append xs (build-list (- len (length xs)) (λ _ fill)))]))

           (define (hard-args->scalars)
             (define xs (args->scalars))
             (unless (= (length xs) len)
               (raise-argument-error
                (string->symbol (format "~a~a" '*vec (or len "")))
                (format "exactly ~a values" len)
                xs))
             xs)

           (define (args->scalars)
             (apply append (for/list ([a (in-list as)])
                             (if (*vec? a) (*vec->list a) (list a)))))

           (define xs
             (cond [(and len (not (eq? fill 'no-fill))) (easy-args->scalars)]
                   [len (hard-args->scalars)]
                    [else (args->scalars)]))

           (define data (apply make-*vec-data xs))
           (make-*vec data
                      (or len (array-length data))
                      (and len (eq? 'no-fill fill))
                      fill))
         '*vec)
        '(function *vec) (format "<pkgs>/glm/~a.rkt" '*vec) '*vec #f))

     (define/contract (make-*vec-data . args)
       (->* (*vec-native?) #:rest (listof *vec-native?) array?)
       (define len (length args))
       (define data (ptr-ref (malloc (_*vec len) 'atomic) (_*vec len) 0))
       (for ([k (in-range len)]
             [x (in-list (map *vec-to-scalar args))])
         (array-set! data k x))
       data)

     (define/contract (*vec-copy v) (-> *vec? *vec?)
       (apply (*vec-constructor v) (*vec->list v)))

     (define/contract (*vec-name v) (-> *vec? symbol?)
       (string->symbol (format "~a~a" '*vec (*vec-length v))))

     (define/contract (*vec-ref v i)
       (-> *vec? exact-nonnegative-integer? *vec-native?)
       (*vec-to-native (array-ref (*vec-data v) i)))

     (define/contract (*vec-set! v i x)
       (-> *vec? exact-nonnegative-integer? *vec-native? void?)
       (array-set! (*vec-data v) i (*vec-to-scalar x)))

     (define/contract (*vec->list v)
       (-> *vec? (listof *vec-native?))
       (sequence->list (in-*vec v)))

     (define/contract (*vec->ffi-vector v)
       (-> *vec? ffi-vector?)
       (cast (array-ptr (*vec-data v)) _pointer (_ffi-vector o (*vec-length v))))

     (define/contract (in-*vec v [start 0] [stop #f] [step 1])
       (->* (*vec?) (exact-nonnegative-integer?
                     (or/c exact-integer? #f)
                     (and/c exact-integer? (not/c zero?)))
            sequence?)
       (sequence-map (compose *vec-to-native from-scalar)
                     (in-array (*vec-data v) start stop step)))

     (define-simple-macro (for/*vec (~optional (~seq #:length len))
                                    (~optional (~seq #:fill fill))
                                    (for-clause ...) body ...)
       (apply *vec
              (~? (~@ #:length len))
              (~? (~@ #:fill fill))
              (let ([num-xs 0])
                (for/list (for-clause
                           ...
                           (~? (~@ #:break (>= num-xs len))))
                  (set! num-xs (add1 num-xs))
                  body ...))))

     (define-simple-macro (for*/*vec (~optional (~seq #:length len))
                                     (~optional (~seq #:fill fill))
                                     (for-clause ...) body ...)
       (apply *vec
              (~? (~@ #:length len))
              (~? (~@ #:fill fill))
              (let ([num-xs 0])
                (for*/list (for-clause
                            ...
                            (~? (~@ #:break (>= num-xs len))))
                  (set! num-xs (add1 num-xs))
                  body ...))))

     (define/contract (*vec-constructor v)
       (-> *vec? (-> (or/c *vec? *vec-native?) ... *vec?))
       (curry *vec #:length (*vec-length v) #:fill (*vec-fill v)))

     (define/contract (*vec=! v u)
       (-> *vec? *vec? void?)
       (for ([i (in-range (*vec-length v))]
             [u-i (in-*vec u)])
         (*vec-set! v i u-i)))

     (define/contract ((make-*vec-binop op x0) . args)
       (-> (-> *vec-native? *vec-native? *vec-native?)
           *vec-native?
           (unconstrained-domain-> (or/c *vec? *vec-native?)))
       (define (vec-vec v1 v2)
         (apply (*vec-constructor v1)
                (for/list ([x1 (in-*vec v1)]
                           [x2 (in-*vec v2)])
                  (op x1 x2))))
       (define vec-binop
         (case-lambda
           [(a) (if (*vec? a)
                    (vec-binop ((*vec-constructor a) x0) a)
                    (op x0 a))]
           [(a b) (cond [(and (*vec? a) (*vec? b)) (vec-vec a b)]
                        [(*vec? a) (vec-vec a ((*vec-constructor a) b))]
                        [(*vec? b) (vec-vec ((*vec-constructor b) a) b)]
                        [else (op a b)])]
           [(a b . cs) (apply vec-binop (vec-binop a b) cs)]))
       (apply vec-binop args))

     (define-simple-macro (define-*vec-type name:id #:length len:nat)
       #:with name? (format-id #'name "~a?" #'name)
       #:with name* ((make-syntax-introducer) (format-id #'name "~a*" #'name))
       (...
        (begin
          (define/contract (name? a) predicate/c
            (and (*vec? a) (= (*vec-length a) len)))
          (define-match-expander name
            (syntax-parser
              [(_ x ... #:rest tail) #'(? name? (app *vec->list (list-rest x ... tail)))]
              [(_ x ...) #'(? name? (app *vec->list (list-rest x ... _)))])
            (make-rename-transformer #'name*))
          (define/contract name*
            (-> (or/c *vec? *vec-native?) ... name?)
            (procedure-rename (curry *vec #:length len) 'name)))))

     (define-*vec-type *vec1 #:length 1)
     (define-*vec-type *vec2 #:length 2)
     (define-*vec-type *vec3 #:length 3)
     (define-*vec-type *vec4 #:length 4))))
