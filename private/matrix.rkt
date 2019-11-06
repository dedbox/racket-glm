#lang racket/base

(require (except-in ffi/unsafe ->)
         glm/vector-types
         math/flonum
         racket/contract
         racket/format
         racket/function
         racket/generic
         racket/list
         racket/match
         racket/pretty
         racket/string
         syntax/parse/define
         (for-syntax racket/base
                     racket/syntax))

(provide (all-defined-out))

(define-simple-macro (define-matrix-type *mat:id
                       #:vector-type *vec:id
                       #:c-type     _*ctype:id
                       #:ffi-vector ffi-vector:id)
  #:with  current-*vec-precision (format-id #'*vec  "current-~a-precision" #'*vec)
  #:with *mat-column-constructor (format-id #'*mat "~a-column-constructor" #'*mat)
  #:with    *mat-row-constructor (format-id #'*mat    "~a-row-constructor" #'*mat)
  #:with        *mat-constructor (format-id #'*mat        "~a-constructor" #'*mat)
  #:with   *mat-column-predicate (format-id #'*mat   "~a-column-predicate" #'*mat)
  #:with      *mat-row-predicate (format-id #'*mat      "~a-row-predicate" #'*mat)
  #:with          *mat-predicate (format-id #'*mat          "~a-predicate" #'*mat)
  #:with       _ffi-vector (format-id #'ffi-vector "_~a" #'ffi-vector)
  #:with       ffi-vector? (format-id #'ffi-vector "~a?" #'ffi-vector)
  #:with  *mat->ffi-vector (format-id #'*mat "~a->~a" #'*mat #'ffi-vector)
  #:with make-*mat        (format-id #'*mat "make-~a"        #'*mat)
  #:with make-*mat-data   (format-id #'*mat "make-~a-data"   #'*mat)
  #:with *mat-data        (format-id #'*mat "~a-data"        #'*mat)
  #:with *glm-mat         (format-id #'*mat "glm:~a"         #'*mat)
  #:with *mat~            (format-id #'*mat "~~~a~~"         #'*mat)
  #:with _*mat            (format-id #'*mat "_~a"            #'*mat)
  #:with *mat?            (format-id #'*mat "~a?"            #'*mat)
  #:with *mat-length      (format-id #'*mat "~a-length"      #'*mat)
  #:with *mat-num-cols    (format-id #'*mat "~a-num-cols"    #'*mat)
  #:with *mat-num-rows    (format-id #'*mat "~a-num-rows"    #'*mat)
  #:with *mat-copy        (format-id #'*mat "~a-copy"        #'*mat)
  #:with *mat-column      (format-id #'*mat "~a-column"      #'*mat)
  #:with *mat-row         (format-id #'*mat "~a-row"         #'*mat)
  #:with *mat-ref         (format-id #'*mat "~a-ref"         #'*mat)
  #:with *mat-name        (format-id #'*mat "~a-name"        #'*mat)
  #:with *mat-columns     (format-id #'*mat "~a-columns"     #'*mat)
  #:with *mat-columns~    (format-id #'*mat "~~~a-columns~~" #'*mat)
  #:with *mat-rows        (format-id #'*mat "~a-rows"        #'*mat)
  #:with *mat-set-column! (format-id #'*mat "~a-set-column!" #'*mat)
  #:with *mat-set-row!    (format-id #'*mat "~a-set-row!"    #'*mat)
  #:with *mat-set!        (format-id #'*mat "~a-set!"        #'*mat)
  #:with *mat->list       (format-id #'*mat "~a->list"       #'*mat)
  #:with in-*mat-columns  (format-id #'*mat "in-~a-columns"  #'*mat)
  #:with in-*mat-rows     (format-id #'*mat "in-~a-rows"     #'*mat)
  #:with in-*mat          (format-id #'*mat "in-~a"          #'*mat)
  #:with  for/*mat        (format-id #'*mat  "for/~a"        #'*mat)
  #:with for*/*mat        (format-id #'*mat "for*/~a"        #'*mat)
  #:with *mat=!           (format-id #'*mat "~a=!"           #'*mat)
  #:with make-*mat-binop  (format-id #'*mat "make-~a-binop"  #'*mat)
  #:with *mat+            (format-id #'*mat "~a+"            #'*mat)
  #:with *mat-            (format-id #'*mat "~a-"            #'*mat)
  #:with *mat*            (format-id #'*mat "~a*"            #'*mat)
  #:with *mat/            (format-id #'*mat "~a/"            #'*mat)
  #:with *mat+=!          (format-id #'*mat "~a+=!"          #'*mat)
  #:with *mat-=!          (format-id #'*mat "~a-=!"          #'*mat)
  #:with *mat*=!          (format-id #'*mat "~a*=!"          #'*mat)
  #:with *mat/=!          (format-id #'*mat "~a/=!"          #'*mat)
  #:with *mat++!          (format-id #'*mat "~a++!"          #'*mat)
  #:with *mat--!          (format-id #'*mat "~a--!"          #'*mat)
  #:with ++*mat!          (format-id #'*mat "++~a!"          #'*mat)
  #:with --*mat!          (format-id #'*mat "--~a!"          #'*mat)
  #:with define-*mat-type (format-id #'*mat "define-~a-type" #'*mat)
  #:with *mat2?           (format-id #'*mat "~a2?"           #'*mat)
  #:with *mat3?           (format-id #'*mat "~a3?"           #'*mat)
  #:with *mat4?           (format-id #'*mat "~a4?"           #'*mat)
  #:with *mat2            (format-id #'*mat "~a2"            #'*mat)
  #:with *mat3            (format-id #'*mat "~a3"            #'*mat)
  #:with *mat4            (format-id #'*mat "~a4"            #'*mat)
  #:with *mat2x2          (format-id #'*mat "~a2x2"          #'*mat)
  #:with *mat2x3          (format-id #'*mat "~a2x3"          #'*mat)
  #:with *mat2x4          (format-id #'*mat "~a2x4"          #'*mat)
  #:with *mat3x2          (format-id #'*mat "~a3x2"          #'*mat)
  #:with *mat3x3          (format-id #'*mat "~a3x3"          #'*mat)
  #:with *mat3x4          (format-id #'*mat "~a3x4"          #'*mat)
  #:with *mat4x2          (format-id #'*mat "~a4x2"          #'*mat)
  #:with *mat4x3          (format-id #'*mat "~a4x3"          #'*mat)
  #:with *mat4x4          (format-id #'*mat "~a4x4"          #'*mat)
  #:with _*mat2           (format-id #'*mat "_~a2"           #'*mat)
  #:with _*mat3           (format-id #'*mat "_~a3"           #'*mat)
  #:with _*mat4           (format-id #'*mat "_~a4"           #'*mat)
  #:with _*mat2x2         (format-id #'*mat "_~a2x2"         #'*mat)
  #:with _*mat2x3         (format-id #'*mat "_~a2x3"         #'*mat)
  #:with _*mat2x4         (format-id #'*mat "_~a2x4"         #'*mat)
  #:with _*mat3x2         (format-id #'*mat "_~a3x2"         #'*mat)
  #:with _*mat3x3         (format-id #'*mat "_~a3x3"         #'*mat)
  #:with _*mat3x4         (format-id #'*mat "_~a3x4"         #'*mat)
  #:with _*mat4x2         (format-id #'*mat "_~a4x2"         #'*mat)
  #:with _*mat4x3         (format-id #'*mat "_~a4x3"         #'*mat)
  #:with _*mat4x4         (format-id #'*mat "_~a4x4"         #'*mat)
  #:with _*vec            (format-id #'*vec "_~a"            #'*vec)
  #:with *vec?            (format-id #'*vec "~a?"            #'*vec)
  #:with *vec-name        (format-id #'*vec "~a-name"        #'*vec)
  #:with *vec->list       (format-id #'*vec "~a->list"       #'*vec)
  #:with for/*vec         (format-id #'*vec "for/~a"         #'*vec)
  #:with in-*vec          (format-id #'*vec "in-~a"          #'*vec)
  #:with make-*vec        (format-id #'*vec "make-~a"        #'*vec)
  #:with *vec-set!        (format-id #'*vec "~a-set!"        #'*vec)
  #:with *vec-ref         (format-id #'*vec "~a-ref"         #'*vec)
  #:with *vec-length      (format-id #'*vec "~a-length"      #'*vec)
  #:with *vec=!           (format-id #'*vec "~a=!"           #'*vec)
  #:with *vec-copy        (format-id #'*vec "~a-copy"        #'*vec)
  #:with *vec+            (format-id #'*vec "~a+"            #'*vec)
  #:with *vec-            (format-id #'*vec "~a-"            #'*vec)
  #:with *vec*            (format-id #'*vec "~a*"            #'*vec)
  #:with *vec/            (format-id #'*vec "~a/"            #'*vec)
  #:with *vec++!          (format-id #'*vec "~a++!"          #'*vec)
  #:with *vec--!          (format-id #'*vec "~a--!"          #'*vec)
  #:with ++*vec!          (format-id #'*vec "++~a!"          #'*vec)
  #:with --*vec!          (format-id #'*vec "--~a!"          #'*vec)
  #:with *vec4            (format-id #'*vec "~a4"            #'*vec)
  #:with *mat-inverse (format-id #'*mat "~a-inverse" #'*mat)

  (...
   (begin

     (define (_*mat cols rows) (_array (_*vec rows) cols))

     (define-match-expander *mat
       (syntax-rules ()
         [(_ #:cols cols #:rows rows x ... #:rest tail)
          (? *mat? (and (~? (app *mat-num-cols cols))
                        (~? (app *mat-num-rows rows))
                        (app *mat->list (list-rest x ... tail))))]
         [(_ #:rows rows x ... #:rest tail)
          (*mat #:cols _ #:rows rows x ... #:rest tail)]
         [(_ #:rows rows x ...)
          (*mat #:cols _ #:rows rows x ... #:rest (list))]
         [(_ #:cols cols x ... #:rest tail)
          (*mat #:cols cols #:rows _ x ... #:rest tail)]
         [(_ #:cols cols x ...)
          (*mat #:cols cols #:rows _ x ... #:rest (list))]
         [(_ x ... #:rest tail) (*mat #:rows _ #:cols _ x ... #:rest tail)]
         [(_ x0 x ...) (*mat #:cols _ #:rows _ x0 x ... #:rest (list))])
       (make-rename-transformer #'*mat~))

     (define-match-expander *mat-columns
       (syntax-parser
         [(_ v ... #:rest tail) #'(? *mat? (app *mat-columns (list-rest v ... tail)))]
         [(_ v ...) #'(? *mat? (app *mat-columns (list v ...)))])
       (make-rename-transformer #'*mat-columns~))

     (struct *mat (data num-cols num-rows)
       #:transparent
       #:name *glm-mat
       #:constructor-name make-*mat
       #:methods gen:equal+hash
       [(define/generic gen-hash-proc hash-proc)
        (define/generic gen-hash2-proc hash2-proc)
        (define (equal-proc m1 m2 _)
          (and (= (*mat-num-cols m1) (*mat-num-cols m2))
               (= (*mat-num-rows m1) (*mat-num-rows m2))
               (for/and ([v1 (in-*mat-columns m1)]
                         [v2 (in-*mat-columns m2)])
                 (equal? v1 v2))))
        (define (hash-proc m _) (gen-hash-proc (*mat-data m)))
        (define (hash2-proc m _) (gen-hash2-proc (*mat-data m)))]
       #:methods gen:custom-write
       [(define (write-proc m port mode)
          (case mode
            [(#t #f) (fprintf port "#<~a>" (*mat-name m))]
            [(1) ((if (pretty-printing) pretty-print print)
                  `(,@(if (and (>= (*mat-num-cols m) 2) (<= (*mat-num-cols m) 4)
                               (>= (*mat-num-rows m) 2) (<= (*mat-num-rows m) 4))
                          (list (*mat-name m))
                          `(*mat #:cols ,(*mat-num-cols m)
                                 #:rows ,(*mat-num-rows m)))
                    ,@(*mat-columns m)) port mode)]
            [(0)
             (define (stringify x)
               (if (rational? x)
                   (~r #:precision (current-*vec-precision) x)
                   (~a x)))
             (define col-strs (for/list ([v (in-*mat-columns m)])
                                (map stringify (*vec->list v))))
             (define col-widths (for/list ([ss (in-list col-strs)])
                                  (apply max (map string-length ss))))
             (define row-strs
               (for/list ([v (in-*mat-rows m)])
                 (string-join
                  (for/list ([x (in-*vec v)]
                             [w (in-list col-widths)])
                    (define str (stringify x))
                    (define pad (build-list (- w (string-length str)) (λ _ #\space)))
                    (format "~a~a" (apply string pad) str))
                  " | ")))
             (for ([ss (in-list row-strs)])
               (fprintf port "[ ~a ]\n" ss))]))])

     (define/contract *mat~
       (->* (#:rows exact-positive-integer?)
            (#:cols (or/c exact-positive-integer? #f)
             #:fill (or/c real? #f))
            #:rest (listof (or/c *mat? *vec? real?))
            *mat?)
       (procedure-rename
        (λ (#:rows num-rows #:cols [num-cols #f] #:fill [fill 0] . as)
          (define M num-rows)

          (define (column-vector . args)
            (apply *vec #:length M #:fill fill args))

          (define (args->scalars)
            (define xs
              (apply append (for/list ([a (in-list as)])
                              (cond [(*mat? a) (*mat->list a)]
                                    [(*vec? a) (*vec->list a)]
                                    [else (list a)]))))
            (define-values (xs-cols xs-rem) (quotient/remainder (length xs) num-rows))
            (unless (or fill num-cols (zero? xs-rem) (<= (length as) 1))
              (raise-argument-error
               '*mat (if num-cols
                         (format "~a component values" (* num-cols num-rows))
                         (format "a multiple of ~a component values" num-rows))
               xs))
            (define N (cond [num-cols num-cols]
                            [(null? xs) num-rows]
                            [(zero? xs-rem) xs-cols]
                            [else (add1 xs-cols)]))
            (values N xs))

          (define (scalars->columns N xs)
            (cond [(= (length xs) 0) (null->columns N)]
                  [(= (length xs) 1) (singleton->columns N (car xs))]
                  [(< (length xs) (* M N)) (if fill (fill->columns N xs) null)]
                  [(= (length xs) (* M N)) (components->columns N xs)]
                  [else null]))

          (define (null->columns N)
            (singleton->columns N 1))

          (define (singleton->columns N x)
            (for/list ([i (in-range N)])
              (define v (column-vector))
              (when (< i M) (*vec-set! v i x))
              v))

          (define (fill->columns N xs)
            (define-values (vs _)
              (for/fold ([vs null]
                         [xs xs])
                        ([i (in-range N)])
                (define-values (a b)
                  (if (< (length xs) M)
                      (let ([ys (build-list (- M (length xs)) (λ _ fill))])
                        (values (append xs ys) null))
                      (split-at xs M)))
                (values (append vs (list (apply column-vector a))) b)))
            vs)

          (define (components->columns N xs)
            (define-values (vs _)
              (for/fold ([vs null]
                         [xs xs])
                        ([i (in-range N)])
                (define-values (head tail) (split-at xs M))
                (values (append vs (list (apply column-vector head))) tail)))
            vs)

          (define-values (N vs)
            (cond [(= (length as) 1)
                   (cond [(*mat? (car as))
                          (define us (null->columns (or num-cols num-rows)))
                          (define vs (for/list ([v (in-*mat-columns (car as))])
                                       (column-vector v)))
                          (values (length us)
                                  (if (< (length vs) (length us))
                                      (append vs (drop us (length vs)))
                                      (take vs (length us))))]
                         [(*vec? (car as))
                          (define N* (or num-cols num-rows))
                          (values N* (for/list ([_ (in-range N*)])
                                       (column-vector (car as))))]
                         [else
                          (define N* (or num-cols num-rows))
                          (values N* (singleton->columns N* (car as)))])]
                  [(and num-cols (= (length as) num-cols))
                   (define vs (for/list ([a (in-list as)])
                                (column-vector a)))
                   (values (length vs) vs)]
                  [else
                   (define-values (N* xs*) (args->scalars))
                   (values N* (scalars->columns N* xs*))]))

          (define vals (apply append (map *vec->list vs)))
          (when (null? vals)
            (if fill
                (apply raise-arity-error 'mat (build-list (add1 (* N M)) values) as)
                (apply raise-arity-error 'mat `(0 1 ,(* N M)) as)))

          (make-*mat (apply make-*mat-data N M vals) N M))

        '*mat))

     (define/contract (*mat-length m) (-> *mat? exact-positive-integer?)
       (*mat-num-cols m))

     (define/contract (make-*mat-data cols rows . xs)
       (->* (exact-positive-integer?
             exact-positive-integer? real?) #:rest (listof real?) array?)
       (define data (ptr-ref (malloc (_*mat cols rows) 'atomic) (_*mat cols rows) 0))
       (define vs (for/list ([a (in-array data)]) (make-*vec a rows #t 0)))
       (for ([x (in-list xs)]
             [k (in-naturals)])
         (define-values (col row) (quotient/remainder k rows))
         (*vec-set! (list-ref vs col) row x))
       data)

     (define/contract (*mat-copy m) (-> *mat? *mat?)
       (apply (*mat-constructor m) (*mat->list m)))

     (define/contract (*mat-name m) (-> *mat? symbol?)
       (string->symbol
        (if (= (*mat-num-cols m) (*mat-num-rows m))
            (format "~a~a" '*mat (*mat-num-rows m))
            (format "~a~ax~a" '*mat (*mat-num-cols m) (*mat-num-rows m)))))

     (define/contract *mat-ref
       (case-> (-> *mat? exact-nonnegative-integer? real?)
               (-> *mat? exact-nonnegative-integer? exact-nonnegative-integer? real?))
       (case-lambda
         [(m k)
          (define-values (col row) (quotient/remainder k (*mat-num-rows m)))
          (*mat-ref m col row)]
         [(m col row)
          (array-ref (*mat-data m) col row)]))

     (define/contract (*mat-row m k) (-> *mat? exact-nonnegative-integer? *vec?)
       (apply (*mat-row-constructor m)
              (for/list ([v (in-*mat-columns m)])
                (*vec-ref v k))))

     (define/contract (*mat-column m k) (-> *mat? exact-nonnegative-integer? *vec?)
       (list-ref (*mat-columns m) k))

     (define/contract *mat-set!
       (case-> (-> *mat? exact-nonnegative-integer? real? void?)
               (-> *mat? exact-nonnegative-integer? exact-nonnegative-integer? real? void?))
       (case-lambda
         [(m k x)
          (define-values (col row) (quotient/remainder k (*mat-num-rows m)))
          (*mat-set! m col row x)]
         [(m col row x)
          (*vec-set! (*mat-column m col) row x)]))

     (define/contract (*mat-set-row! m row v)
       (-> *mat? exact-nonnegative-integer? *vec? void?)
       (for ([x (in-*vec v)]
             [j (in-naturals)])
         (*mat-set! m j row x)))

     (define/contract (*mat-set-column! m col v)
       (-> *mat? exact-nonnegative-integer? *vec? void?)
       (*vec=! (*mat-column m col) v))

     (define/contract (*mat->list m) (-> *mat? (listof real?))
       (for*/list ([v (in-*mat-columns m)] [x (in-*vec v)]) x))

     (define/contract (*mat-rows m) (-> *mat? (listof *vec?))
       (define vs (*mat-columns m))
       (for/list ([j (in-range (*mat-num-rows m))])
         (apply *vec #:length (*mat-num-cols m) (map (curryr *vec-ref j) vs))))

     (define/contract *mat-columns~ (-> *mat? (listof *vec?))
       (procedure-rename
        (λ (m)
          (for/list ([a (in-array (*mat-data m))]
                     [i (in-naturals)]
                     #:break (>= i (*mat-num-cols m)))
            (make-*vec a (*mat-num-rows m) #t 0)))
        '*mat-columns))

     (define/contract (*mat->ffi-vector m) (-> *mat? ffi-vector?)
       (cast (array-ptr (*mat-data m))
             _pointer
             (_ffi-vector o (* (*mat-num-cols m) (*mat-num-rows m)))))

     (define/contract (in-*mat-columns m) (-> *mat? sequence?)
       (in-list (*mat-columns m)))

     (define/contract (in-*mat-rows m) (-> *mat? sequence?)
       (in-list (*mat-rows m)))

     (define/contract (in-*mat m) (-> *mat? sequence?)
       (in-list (*mat->list m)))

     (define-simple-macro (for/*mat
                              (~optional (~seq #:cols cols:expr))
                              #:rows rows:expr
                              (~optional (~seq #:fill fill:expr))
                            (for-clause ...) body ...)
       (apply *mat
              (~? (~@ #:cols cols))
              #:rows rows
              (~? (~@ #:fill fill))
              (let ([num-xs 0])
                (for/list (for-clause
                           ...
                           (~? (~@ #:break (>= num-xs (* rows cols)))))
                  (set! num-xs (add1 num-xs))
                  body ...))))

     (define-simple-macro (for*/*mat
                              (~optional (~seq #:cols cols:expr))
                              #:rows rows:expr
                              (~optional (~seq #:fill fill:expr))
                            (for-clause ...) body ...)
       (apply *mat
              (~? (~@ #:cols cols))
              #:rows rows
              (~? (~@ #:fill fill))
              (let ([num-xs 0])
                (for*/list (for-clause
                            ...
                            (~? (~@ #:break (>= num-xs (* rows cols)))))
                  (set! num-xs (add1 num-xs))
                  body ...))))

     (define/contract ((*mat-predicate m) a) (-> *mat? predicate/c)
       (and (*mat? a)
            (= (*mat-num-cols m) (*mat-num-cols a))
            (= (*mat-num-rows m) (*mat-num-rows a))))

     (define/contract ((*mat-row-predicate m) a) (-> *mat? predicate/c)
       (and (*vec? a) (= (*vec-length a) (*mat-num-cols m))))

     (define/contract ((*mat-column-predicate m) a) (-> *mat? predicate/c)
       (and (*vec? a) (= (*vec-length a) (*mat-num-rows m))))

     (define/contract (*mat-constructor m)
       (-> *mat? (unconstrained-domain-> *mat?))
       (curry *mat #:cols (*mat-num-cols m) #:rows (*mat-num-rows m)))

     (define/contract (*mat-row-constructor m)
       (-> *mat? (unconstrained-domain-> *vec?))
       (curry *vec #:length (*mat-num-cols m)))

     (define/contract (*mat-column-constructor m)
       (-> *mat? (unconstrained-domain-> *vec?))
       (curry *vec #:length (*mat-num-rows m)))

     (define/contract (*mat=! m1 m2) (-> *mat? *mat? void?)
       (unless (and (= (*mat-num-cols m1) (*mat-num-cols m2))
                    (= (*mat-num-rows m1) (*mat-num-rows m2)))
         (error '*mat=! "matrices are not the same length"))
       (memcpy (array-ptr (*mat-data m1))
               (array-ptr (*mat-data m2))
               (* (*mat-num-cols m1) (*mat-num-rows m1))
               _*ctype))

     (define ((make-*mat-binop op-name op vec-op x0) . args)
       (define (mat-mat m1 m2)
         (apply (*mat-constructor m1)
                (for/list ([v1 (in-*mat-columns m1)]
                           [v2 (in-*mat-columns m2)])
                  (vec-op v1 v2))))
       (define (unary-mat m)
         (apply (*mat-constructor m) (for/list ([xk (in-*mat-columns m)]) (vec-op xk))))
       (define (mat-vec m v)
         (apply (*mat-constructor m) (for/list ([vk (in-*mat-columns m)]) (vec-op vk v))))
       (define (vec-mat v m)
         (apply (*mat-constructor m) (for/list ([vk (in-*mat-columns m)]) (vec-op v vk))))
       (define mat-binop
         (case-lambda
           [(a) (cond [(*mat? a) (unary-mat a)]
                      [(*vec? a) (vec-op a)]
                      [else (op a)])]
           [(a b) (cond [(and (*mat? a) (*mat? b)) (mat-mat a b)]
                        [(*mat? a) (mat-vec a b)]
                        [(*mat? b) (vec-mat a b)]
                        [else (vec-op a b)])]
           [(a . bs) (for/fold ([x (cond [(*mat? a) (*mat-copy a)]
                                         [(*vec? a) (*vec-copy a)]
                                         [else a])])
                               ([y (in-list bs)])
                       (mat-binop x y))]))
       (apply mat-binop args))

     (define *mat+ (make-*mat-binop '+ + *vec+ 0))
     (define *mat- (make-*mat-binop '- - *vec- 0))

     (define (*mat* . args)
       (define (mat*mat m1 m2)
         (unless (= (*mat-num-cols m1) (*mat-num-rows m2))
           (raise-argument-error
            '*mat* (symbol->string
                    (*mat-name (*mat #:cols (*mat-num-cols m1)
                                     #:rows (*mat-num-rows m2)))) m2))
         (for*/*mat #:cols (*mat-num-cols m2)
                    #:rows (*mat-num-rows m1)
             ([v-i (in-*mat-rows m1)]
              [v-j (in-*mat-columns m2)])
           (for/sum ([x (in-*vec (*vec* v-i v-j))]) x)))

       (define (mat*vec m v)
         (unless (= (*vec-length v) (*mat-num-cols m))
           (raise-argument-error
            '*mat* (symbol->string
                    (*vec-name (*vec #:length (*mat-num-cols m)))) v))
         (for/*vec #:length (*mat-num-cols m)
             ([v-i (in-*mat-rows m)])
           (for/sum ([x (in-*vec (*vec* v-i v))]) x)))

       (define (vec*mat v m)
         (unless (= (*vec-length v) (*mat-num-rows m))
           (raise-argument-error
            '*mat* (symbol->string
                    (*vec-name (*vec #:length (*mat-num-rows m)))) v))
         (for/*vec #:length (*mat-num-rows m)
             ([v-j (in-*mat-columns m)])
           (for/sum ([x (in-*vec (*vec* v v-j))]) x)))

       (define (mat*scalar m x)
         (for/*mat #:cols (*mat-num-cols m)
                   #:rows (*mat-num-rows m)
             ([m-x (in-*mat m)])
           (* m-x x)))

       (define (scalar*mat x m)
         (for/*mat #:cols (*mat-num-cols m)
                   #:rows (*mat-num-rows m)
             ([m-x (in-*mat m)])
           (* x m-x)))

       (apply (case-lambda
                [(a)
                 (cond [(*mat? a) (*mat* 1 a)]
                       [(*vec? a) (*vec* a)]
                       [else (* a)])]
                [(a b)
                 ((cond [(and (*mat? a) (*mat? b)) mat*mat]
                        [(and (*mat? a) (*vec? b)) mat*vec]
                        [(and (*vec? a) (*mat? b)) vec*mat]
                        [(*mat? a) mat*scalar]
                        [(*mat? b) scalar*mat]
                        [(or (*vec? a) (*vec? b)) *vec*]
                        [else *])
                  a b)]
                [(a b . cs)
                 (apply *mat* (*mat* a b) cs)])
              args))

     (define (*mat/ . args)
       (define (mat/mat m1 m2)
         (*mat* m1 (*mat-inverse m2)))
       (define (mat/vec m v)
         (*mat* (*mat-inverse m) v))
       (define (vec/mat v m)
         (*mat* v (*mat-inverse m)))
       (define (mat/scalar m x)
         (for/*mat #:cols (*mat-num-cols m)
                   #:rows (*mat-num-rows m)
             ([m-x (in-*mat m)])
           (/ m-x x)))
       (define (scalar/mat x m)
         (for/*mat #:cols (*mat-num-cols m)
                   #:rows (*mat-num-rows m)
             ([m-x (in-*mat m)])
           (/ x m-x)))
       (apply (case-lambda
                [(a)
                 (cond [(*mat? a) (*mat/ 1 a)]
                       [(*vec? a) (*vec/ a)]
                       [else (/ a)])]
                [(a b)
                 ((cond [(and (*mat? a) (*mat? b)) mat/mat]
                        [(and (*mat? a) (*vec? b)) mat/vec]
                        [(and (*vec? a) (*mat? b)) vec/mat]
                        [(*mat? a) mat/scalar]
                        [(*mat? b) scalar/mat]
                        [(or (*vec? a) (*vec? b)) *vec/]
                        [else /])
                  a b)]
                [(a b . cs)
                 (apply *mat/ (*mat/ a b) cs)])
              args))

     (define/contract (*mat+=! m . args)
       (-> *mat? (or/c *mat? *vec? real?) ... void?)
       (*mat=! m (apply *mat+ m args)))

     (define/contract (*mat-=! m . args)
       (-> *mat? (or/c *mat? *vec? real?) ... void?)
       (*mat=! m (apply *mat- m args)))

     (define/contract (*mat*=! m . args)
       (-> *mat? (or/c *mat? *vec? real?) ... void?)
       (*mat=! m (apply *mat* m args)))

     (define/contract (*mat/=! m . args)
       (-> *mat? (or/c *mat? *vec? real?) ... void?)
       (*mat=! m (apply *mat/ m args)))

     (define/contract (++*mat! m) (-> *mat? *mat?)
       (for/*mat #:cols (*mat-num-cols m)
                 #:rows (*mat-num-rows m)
           ([v (in-*mat-columns m)])
         (++*vec! v)))

     (define/contract (--*mat! m) (-> *mat? *mat?)
       (for/*mat #:cols (*mat-num-cols m)
                 #:rows (*mat-num-rows m)
           ([v (in-*mat-columns m)])
         (--*vec! v)))

     (define/contract (*mat++! m) (-> *mat? *mat?)
       (for/*mat #:cols (*mat-num-cols m)
                 #:rows (*mat-num-rows m)
           ([v (in-*mat-columns m)])
         (*vec++! v)))

     (define/contract (*mat--! m) (-> *mat? *mat?)
       (for/*mat #:cols (*mat-num-cols m)
                 #:rows (*mat-num-rows m)
           ([v (in-*mat-columns m)])
         (*vec--! v)))

     (define-simple-macro (define-*mat-type name:id #:cols cols:nat #:rows rows:nat)
       #:with _name (format-id #'name  "_~a" #'name)
       #:with name? (format-id #'name  "~a?" #'name)
       #:with name* ((make-syntax-introducer) (format-id #'name "~a?" #'name))
       (...
        (begin
          (define _name (_*mat rows cols))
          (define/contract (name? a) predicate/c
            (and (*mat? a)
                 (= (*mat-num-cols a) cols)
                 (= (*mat-num-rows a) rows)))
          (define-match-expander name
            (syntax-rules ()
              [(_ x ... #:rest tail) (? name? (app *mat->list (list-rest x ... tail)))]
              [(_ x ...) (? name? (app *mat->list (list-rest x ... _)))])
            (make-rename-transformer #'name*))
          (define/contract name*
            (-> (or/c *mat? *vec? real?) ... name?)
            (procedure-rename (curry *mat #:cols cols #:rows rows) 'name)))))

     (define-*mat-type *mat2 #:cols 2 #:rows 2)
     (define-*mat-type *mat3 #:cols 3 #:rows 3)
     (define-*mat-type *mat4 #:cols 4 #:rows 4)
     (define-*mat-type *mat2x2 #:cols 2 #:rows 2)
     (define-*mat-type *mat2x3 #:cols 2 #:rows 3)
     (define-*mat-type *mat2x4 #:cols 2 #:rows 4)
     (define-*mat-type *mat3x2 #:cols 3 #:rows 2)
     (define-*mat-type *mat3x3 #:cols 3 #:rows 3)
     (define-*mat-type *mat3x4 #:cols 3 #:rows 4)
     (define-*mat-type *mat4x2 #:cols 4 #:rows 2)
     (define-*mat-type *mat4x3 #:cols 4 #:rows 3)
     (define-*mat-type *mat4x4 #:cols 4 #:rows 4)

     (define/contract (*mat-inverse m) (-> *mat? *mat?)
       (define (inverse2)
         (match-define (*mat2 m00 m10 m01 m11) m)
         (define OneOverDeterminant (/ 1 (- (* m00 m11) (* m01 m10))))
         (*mat2 (* OneOverDeterminant m11)
                (* OneOverDeterminant (- m10))
                (* OneOverDeterminant (- m01))
                (* OneOverDeterminant m00)))

       (define (inverse3)
         (match-define (*mat3 m00 m10 m20 m01 m11 m21 m02 m12 m22) m)
         (define OneOverDeterminant
           (/ 1 (+ (*    m00  (- (* m11 m22) (* m12 m21)))
                   (* (- m01) (- (* m10 m22) (* m12 m20)))
                   (*    m02  (- (* m10 m21) (* m11 m20))))))
         (*mat3 (*    OneOverDeterminant  (- (* m11 m22) (* m12 m21)))
                (* (- OneOverDeterminant) (- (* m10 m22) (* m12 m20)))
                (*    OneOverDeterminant  (- (* m10 m21) (* m11 m20)))
                (* (- OneOverDeterminant) (- (* m01 m22) (* m02 m21)))
                (*    OneOverDeterminant  (- (* m00 m22) (* m02 m20)))
                (* (- OneOverDeterminant) (- (* m00 m12) (* m01 m20)))
                (*    OneOverDeterminant  (- (* m01 m12) (* m02 m11)))
                (* (- OneOverDeterminant) (- (* m00 m12) (* m02 m10)))
                (*    OneOverDeterminant  (- (* m00 m11) (* m01 m10)))))

       (define (inverse4)
         (match-define (*mat4 m00 m10 m20 m30
                              m01 m11 m21 m31
                              m02 m12 m22 m32
                              m03 m13 m23 m33) m)

         (define Coef00 (- (* m22 m33) (* m23 m32)))
         (define Coef02 (- (* m21 m33) (* m23 m31)))
         (define Coef03 (- (* m21 m32) (* m22 m31)))

         (define Coef04 (- (* m12 m33) (* m13 m32)))
         (define Coef06 (- (* m11 m33) (* m13 m31)))
         (define Coef07 (- (* m11 m32) (* m12 m31)))

         (define Coef08 (- (* m12 m23) (* m13 m22)))
         (define Coef10 (- (* m11 m23) (* m13 m21)))
         (define Coef11 (- (* m11 m22) (* m12 m21)))

         (define Coef12 (- (* m02 m33) (* m03 m32)))
         (define Coef14 (- (* m01 m33) (* m03 m31)))
         (define Coef15 (- (* m01 m32) (* m02 m31)))

         (define Coef16 (- (* m02 m23) (* m03 m22)))
         (define Coef18 (- (* m01 m23) (* m03 m21)))
         (define Coef19 (- (* m01 m22) (* m02 m21)))

         (define Coef20 (- (* m02 m13) (* m03 m12)))
         (define Coef22 (- (* m01 m13) (* m03 m11)))
         (define Coef23 (- (* m01 m12) (* m02 m11)))

         (define Fac0 (*vec4 Coef00 Coef00 Coef02 Coef03))
         (define Fac1 (*vec4 Coef04 Coef04 Coef06 Coef07))
         (define Fac2 (*vec4 Coef08 Coef08 Coef10 Coef11))
         (define Fac3 (*vec4 Coef12 Coef12 Coef14 Coef15))
         (define Fac4 (*vec4 Coef16 Coef16 Coef18 Coef19))
         (define Fac5 (*vec4 Coef20 Coef20 Coef22 Coef23))

         (define Vec0 (*vec4 m01 m00 m00 m00))
         (define Vec1 (*vec4 m11 m10 m10 m10))
         (define Vec2 (*vec4 m21 m20 m20 m20))
         (define Vec3 (*vec4 m31 m30 m30 m30))

         (define Inv0 (*vec+ (*vec- (*vec* Vec1 Fac0) (*vec* Vec2 Fac1)) (*vec* Vec3 Fac2)))
         (define Inv1 (*vec+ (*vec- (*vec* Vec0 Fac0) (*vec* Vec2 Fac3)) (*vec* Vec3 Fac4)))
         (define Inv2 (*vec+ (*vec- (*vec* Vec0 Fac1) (*vec* Vec1 Fac3)) (*vec* Vec3 Fac5)))
         (define Inv3 (*vec+ (*vec- (*vec* Vec0 Fac2) (*vec* Vec1 Fac4)) (*vec* Vec2 Fac5)))

         (define SignA (*vec4 +1 -1 +1 -1))
         (define SignB (*vec4 -1 +1 -1 +1))

         (define Inverse (*mat4 (*vec* Inv0 SignA)
                                (*vec* Inv1 SignB)
                                (*vec* Inv2 SignA)
                                (*vec* Inv3 SignB)))

         (define Row0 (*vec4 (*mat-ref Inverse 0 0)
                             (*mat-ref Inverse 1 0)
                             (*mat-ref Inverse 2 0)
                             (*mat-ref Inverse 3 0)))

         (define Dot0 (*vec* (*mat-column m 0) Row0))
         (define Dot1 (for/sum ([x (in-*vec Dot0)]) x))

         (define OneOverDeterminant (/ 1 Dot1))

         (*mat* Inverse OneOverDeterminant))

       (cond [(*mat2? m) (inverse2)]
             [(*mat3? m) (inverse3)]
             [(*mat4? m) (inverse4)]))

     )))
