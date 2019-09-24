#lang racket/base

(require (except-in ffi/unsafe ->)
         ffi/vector
         glm/vec
         math/flonum
         racket/contract
         racket/format
         racket/function
         racket/generic
         racket/list
         racket/pretty
         racket/sequence
         racket/string
         (for-syntax racket/base
                     racket/syntax))

(provide (all-defined-out))

(define (_mat rows cols) (_array (_vec cols) rows))

(define current-mat-precision (make-parameter 2))

(struct mat (data num-rows num-cols)
  #:transparent
  #:name glm:mat
  #:constructor-name make-mat
  #:methods gen:equal+hash
  [(define/generic gen-hash-proc hash-proc)
   (define/generic gen-hash2-proc hash2-proc)
   (define (equal-proc m1 m2 _)
     (and (= (mat-num-cols m1) (mat-num-cols m2))
          (= (mat-num-rows m1) (mat-num-rows m2))
          (for/and ([v1 (in-mat-columns m1)]
                    [v2 (in-mat-columns m2)])
            (equal? v1 v2))))
   (define (hash-proc m _) (gen-hash-proc (mat-data m)))
   (define (hash2-proc m _) (gen-hash2-proc (mat-data m)))]
  #:methods gen:custom-write
  [(define (write-proc m port mode)
     (case mode
       [(#t #f) (fprintf port "#<~a>" (mat-name m))]
       [(1) ((if (pretty-printing) pretty-print print)
             `(,(mat-name m) ,@(mat-columns m)) port mode)]
       [(0)
        (define (stringify x)
          (if (rational? x)
              (~r #:precision (current-mat-precision) x)
              (~a x)))
        (define col-strs (for/list ([v (in-mat-columns m)])
                           (map stringify (vec->list v))))
        (define col-widths (for/list ([ss (in-list col-strs)])
                             (apply max (map string-length ss))))
        (define row-strs 
          (for/list ([v (in-mat-rows m)])
            (string-join
             (for/list ([x (in-vec v)]
                        [w (in-list col-widths)])
               (define str (stringify x))
               (define pad (build-list (- w (string-length str)) (λ _ #\space)))
               (format "~a~a" (apply string pad) str))
             " | ")))
        (for ([ss (in-list row-strs)])
          (fprintf port "[ ~a ]\n" ss))]))])

(define/contract (mat #:rows num-rows #:cols [num-cols num-rows] . as)
  (->* (#:rows (or/c exact-nonnegative-integer? #f))
       (#:cols (or/c exact-nonnegative-integer? #f))
       #:rest (listof (or/c mat? vec? number?))
       mat?)
  (define len (* num-rows num-cols))
  (define as-len (length as))
  (define vs
    (cond
      [(= as-len 0) (for/list ([k num-cols])
                      (define v (vec #:length num-rows))
                      (when (< k num-rows) (vec-set! v k 1))
                      v)]
      [(= as-len 1)
       (cond [(mat? (car as))
              (map (curry vec #:length num-rows) (mat-columns (car as)))]
             [(vec? (car as))
              (build-list num-cols (λ _ (vec #:length num-rows (car as))))]
             [else (for/list ([k num-cols])
                     (define v (vec #:length num-rows))
                     (when (< k num-rows) (vec-set! v k (car as)))
                     v)])]
      [(= as-len len)
       (let loop ([as as])
         (if (null? as)
             null
             (cons (apply vec #:length num-rows (take as num-rows))
                   (loop (drop as num-rows)))))]
      [(<= as-len num-cols)
       (map (curry vec #:length num-rows)
            (apply append (for/list ([a (in-list as)])
                            (if (mat? a) (mat-columns a) (list a)))))]
      [else (raise-arity-error 'mat (sort (cons len num-cols
                                                (build-list num-cols values))))]))
  (define vs-len (length vs))
  (define xs
    (apply append
           (cond
             [(= vs-len len) (map vec->list vs)]
             [(= vs-len num-cols) (map vec->list vs)]
             [(> vs-len num-cols) (map vec->list (take vs num-cols))]
             [(< vs-len num-cols)
              (define us (build-list (- num-cols (length vs))
                                     (λ _ (vec #:length num-rows 0))))
              (map vec->list (append vs us))])))
  (define data (apply make-mat-data num-cols num-rows xs))
  (make-mat data num-rows num-cols))

(define/contract (make-mat-data rows cols . xs)
  (->* (exact-nonnegative-integer?
        exact-nonnegative-integer? number?) #:rest (listof number?) array?)
  (define data (ptr-ref (malloc (_mat rows cols) 'atomic) (_mat rows cols) 0))
  (for ([x (in-list xs)]
        [k (in-naturals)])
    (define-values (row col) (quotient/remainder k cols))
    (array-set! data row col (scalar x)))
  data)

(define/contract (mat-copy m)
  (-> mat? mat?)
  (apply (mat-constructor m) (mat->list m)))

(define/contract (mat-length m)
  (-> mat? exact-nonnegative-integer?)
  (* (mat-num-cols m)
     (mat-num-rows m)))

(define/contract (mat-column m k)
  (-> mat? exact-nonnegative-integer? flonum?)
  (list-ref (mat-columns m) k))

(define/contract (mat-row m k)
  (-> mat? exact-nonnegative-integer? vec?)
  (apply (mat-row-constructor m)
         (for/list ([v (in-mat-columns m)])
           (vec-ref v k))))

(define/contract mat-ref
  (case-> (-> mat? exact-nonnegative-integer? flonum?)
          (-> mat? exact-nonnegative-integer? exact-nonnegative-integer? flonum?))
  (case-lambda
    [(m k) (let-values ([(row col) (quotient/remainder k (mat-num-cols m))])
             (mat-ref m row col))]
    [(m row col) (array-ref (mat-data m) row col)]))

(define/contract (mat-name m)
  (-> mat? symbol?)
  (string->symbol
   (if (= (mat-num-rows m) (mat-num-cols m))
       (format "mat~a" (mat-num-rows m))
       (format "mat~ax~a" (mat-num-rows m) (mat-num-cols m)))))

(define/contract ((mat-predicate m) a)
  (-> mat? predicate/c)
  (and (mat? a)
       (= (mat-num-rows m) (mat-num-rows a))
       (= (mat-num-cols m) (mat-num-cols a))))

(define/contract ((mat-column-predicate m) a)
  (-> mat? predicate/c)
  (and (vec? a) (= (vec-length a) (mat-num-rows m))))

(define/contract ((mat-row-predicate m) a)
  (-> mat? predicate/c)
  (and (vec? a) (= (vec-length a) (mat-num-cols m))))

(define/contract (mat-constructor m)
  (-> mat? (unconstrained-domain-> mat?))
  (curry mat #:rows (mat-num-rows m) #:cols (mat-num-cols m)))

(define/contract (mat-column-constructor m)
  (-> mat? (unconstrained-domain-> vec?))
  (curry vec #:length (mat-num-rows m)))

(define/contract (mat-row-constructor m)
  (-> mat? (unconstrained-domain-> vec?))
  (curry vec #:length (mat-num-cols m)))

(define/contract (mat-columns m)
  (-> mat? (listof vec?))
  (for/list ([a (in-array (mat-data m))])
    (make-vec a (mat-num-rows m))))

(define/contract (mat-rows m)
  (-> mat? (listof vec?))
  (define vs (mat-columns m))
  (for/list ([j (in-range (mat-num-rows m))])
    (apply vec (map (curryr vec-ref j) vs))))

(define/contract (in-mat-columns m)
  (-> mat? sequence?)
  (in-list (mat-columns m)))

(define/contract (in-mat-rows m)
  (-> mat? sequence?)
  (in-list (mat-rows m)))

(define/contract (in-mat-data m)
  (-> mat? sequence?)
  (in-list (mat->list m)))

(define/contract (mat-set! m row col x)
  (-> mat? exact-nonnegative-integer? exact-nonnegative-integer? number? void?)
  (array-set! (mat-data m) row col (scalar x)))

(define/contract (mat-set-column! m col v)
  (-> mat? exact-nonnegative-integer? vec? void?)
  (for ([x (in-vec v)]
        [i (in-naturals)])
    (mat-set! m i col x)))

(define/contract (mat-set-row! m row v)
  (-> mat? exact-nonnegative-integer? vec? void?)
  (for ([x (in-vec v)]
        [j (in-naturals)])
    (mat-set! m row j x)))

(define/contract (mat->list m)
  (-> mat? (listof flonum?))
  (apply append (for/list ([v (in-array (mat-data m))])
                  (sequence->list (in-array v)))))

(define/contract (mat->f32vector m)
  (-> mat? f32vector?)
  (cast (array-ptr (mat-data m)) _pointer (_f32vector o (mat-length m))))

(define/contract (mat=! m1 m2)
  (-> mat? mat? void?)
  (unless ((mat-predicate m1) m2)
    (error 'mat=! "matrices aren't the same shape"))
  (memcpy (array-ptr (mat-data m1))
          (array-ptr (mat-data m2))
          (mat-length m1)
          _float))

(define ((make-mat-binop op-name op vec-op x0) . args)
  (define (mat-mat m1 m2)
    (apply (mat-constructor m1)
           (for/list ([v1 (in-mat-columns m1)]
                      [v2 (in-mat-columns m2)])
             (vec-op v1 v2))))
  (define (unary-mat m)
    (apply (mat-constructor m) (for/list ([xk (in-mat-columns m)]) (vec-op xk))))
  (define (mat-vec m v)
    (apply (mat-constructor m) (for/list ([vk (in-mat-columns m)]) (vec-op vk v))))
  (define (vec-mat v m)
    (apply (mat-constructor m) (for/list ([vk (in-mat-columns m)]) (vec-op v vk))))
  (define mat-binop
    (case-lambda
      [(a) (cond [(mat? a) (unary-mat a)]
                 [(vec? a) (vec-op a)]
                 [else (op a)])]
      [(a b) (cond [(and (mat? a) (mat? b)) (mat-mat a b)]
                   [(mat? a) (mat-vec a b)]
                   [(mat? b) (vec-mat a b)]
                   [else (vec-op a b)])]
      [(m . as) (for/fold ([n (mat-copy m)])
                          ([a (in-list as)])
                  (mat-binop n a))]))
  (apply mat-binop args))

(define mat+ (make-mat-binop '+ + vec+ 0))
(define mat- (make-mat-binop '- + vec- 0))

(define/contract (mat+= m . args)
  (-> mat? (or/c mat? vec? number?) ... void?)
  (mat=! m (apply mat+ m args)))

(define/contract (mat-= m . args)
  (-> mat? (or/c mat? vec? number?) ... void?)
  (mat=! m (apply mat- m args)))

(define/contract (mat++ m)
  (-> mat? void?)
  (for-each vec++ (mat-columns m)))

(define/contract (mat-- m)
  (-> mat? void?)
  (for-each vec-- (mat-columns m)))

;;; ----------------------------------------------------------------------------

(define/contract (mat*= m . args)
  (-> mat? (or/c mat? vec? number?) ... void?)
  (mat=! m (apply mat* m args)))

(define/contract mat*
  (case-> (-> mat? mat?)
          (-> (or/c mat? vec? number?)
              (or/c mat? vec? number?)
              (or/c mat? vec? number?))
          (-> (or/c mat? vec? number?)
              #:rest (listof (or/c mat? vec? number?))
              (or/c mat? vec? number?)))
  (let ()

    (define (mat*mat m1 m2)
      (unless (= (mat-num-cols m1) (mat-num-rows m2))
        (error 'mat* "expected a matrix with ~a rows" (mat-num-cols m1)))
      (define m (mat #:rows (mat-num-rows m1) #:cols (mat-num-cols m2) 0))
      (for ([ai (in-mat-rows m1)]
            [i (in-naturals)])
        (for ([bj (in-mat-columns m2)]
              [j (in-naturals)])
          (mat-set! m j i (flsum (vec->list (vec* ai bj))))))
      m)

    (define (mat*vec m v)
      (unless ((mat-column-predicate m) v)
        (error 'mat* "expected vec~a" (mat-num-rows m)))
      (if (mat4? m)
          (mat4*vec m v)
          (apply (mat-column-constructor m)
                 (for/list ([vk (in-mat-rows m)])
                   (flsum (vec->list (vec* vk v)))))))

    (define (mat4*vec m v)
      (define-values (m0 m1 m2 m3) (apply values (mat-columns m)))
      (define-values (v0 v1 v2 v3) (apply values (vec->list v)))
      (define Mov0 (vec4 v0))
      (define Mov1 (vec4 v1))
      (define Mul0 (vec* m0 Mov0))
      (define Mul1 (vec* m1 Mov1))
      (define Add0 (vec+ Mul0 Mul1))
      (define Mov2 (vec4 v2))
      (define Mov3 (vec4 v3))
      (define Mul2 (vec* m2 Mov2))
      (define Mul3 (vec* m3 Mov3))
      (define Add1 (vec+ Mul2 Mul3))
      (define Add2 (vec+ Add0 Add1))
      Add2)

    (define (vec*mat v m)
      (unless ((mat-row-predicate m) v)
        (error 'mat* "expected vec~a" (mat-num-cols m)))
      (if (mat4? m)
          (vec*mat4 v m)
          (apply (mat-column-constructor m)
                 (for/list ([mk (in-mat-rows m)])
                   (flsum (vec->list (vec* mk v)))))))

    (define (vec*mat4 v m)
      (define-values (m00 m01 m02 m03
                      m10 m11 m12 m13
                      m20 m21 m22 m23
                      m30 m31 m32 m33) (apply values (mat->list m)))
      (define-values (v0 v1 v2 v3) (apply values (vec->list v)))
      (vec4 (+ (* m00 v0) (* m01 v1) (* m02 v2) (* m03 v3))
            (+ (* m10 v0) (* m11 v1) (* m12 v2) (* m13 v3))
            (+ (* m20 v0) (* m21 v1) (* m22 v2) (* m23 v3))
            (+ (* m30 v0) (* m31 v1) (* m32 v2) (* m33 v3))))

    (define (mat*scalar m x)
      (apply (mat-constructor m) (for/list ([v (in-mat-columns m)]) (vec* v x))))

    (define (scalar*mat x m)
      (mat*scalar m x))

    (case-lambda
      [(m) m]
      [(a b) ((cond [(and (mat? a) (mat? b)) mat*mat]
                    [(mat? a) (if (vec? b) mat*vec mat*scalar)]
                    [(mat? b) (if (vec? a) vec*mat scalar*mat)]
                    [(or (vec? a) (vec? b)) vec*]
                    [else *])
              a b)]
      [(a b . cs) (mat* a (apply mat* b cs))])))

;;; ----------------------------------------------------------------------------

(define-syntax (define-mat stx)
  (syntax-case stx ()
    [(_ type-id rows cols)
     (identifier? #'type-id)
     (with-syntax ([type-id? (format-id #'type-id "~a?" #'type-id)])
       #'(begin
           (define/contract (type-id? a)
             predicate/c
             (and (mat? a)
                  (= (mat-num-rows a) rows)
                  (= (mat-num-cols a) cols)))
           (define type-id (curry mat #:rows rows #:cols cols))))]))

(define-mat mat2 2 2)
(define-mat mat3 3 3)
(define-mat mat4 4 4)
(define-mat mat2x3 2 3)
(define-mat mat2x4 2 4)
(define-mat mat3x2 3 2)
(define-mat mat3x4 3 4)
(define-mat mat4x2 4 2)
(define-mat mat4x3 4 3)

;;; ----------------------------------------------------------------------------

(module+ test
  (require racket/function
           rackunit)

  (define m (mat3 0.0 0.1 0.2
                  1.0 1.1 1.2
                  2.0 2.1 2.2))

  (test-case "column-major order (3)"
    (check equal? (mat-columns m)
           (list (vec3 0.0 0.1 0.2)
                 (vec3 1.0 1.1 1.2)
                 (vec3 2.0 2.1 2.2))))

  (test-case "mat3 and mat->list are isomorphic"
    (check equal? (apply mat3 (mat->list m)) m))

  (test-case "equal?"
    (check equal? (mat3 0) (mat3 0))
    (check equal? (mat3 1) (mat3 1))
    (check equal? (mat3) (mat3))
    (check (negate equal?) (mat3 1) (mat3 0))
    (check (negate equal?) (mat3 0) (mat3 1))
    (check (negate equal?) (mat3 (vec3 1)) (mat3)))

  (test-case "mat+="
    (define m
      (mat3 (mat3 0))) (check equal? m (mat3 0))
    (mat+= m (mat3 1)) (check equal? m (mat3 1))
    (mat+= m (mat3 1)) (check equal? m (mat3 2)))

  (test-case "mat-="
    (define m
      (mat3 (mat3 1))) (check equal? m (mat3  1))
    (mat-= m (mat3 1)) (check equal? m (mat3  0))
    (mat-= m (mat3 1)) (check equal? m (mat3 -1)))

  (test-case "mat+"
    (check equal? (mat+ (mat3 0)) (mat3 0))
    (check equal? (mat+ (mat3 1)) (mat3 1))
    (check equal? (mat+ (mat3 (vec3 1)) 1) (mat3 (vec3 2)))
    (check equal? (mat+ (mat3 (vec3 1)) (mat3 (vec3 1))) (mat3 (vec3 2)))
    (check equal? (mat+ (mat3 (vec3 1)) 1 (mat3 (vec3 1))) (mat3 (vec3 3)))
    (check equal? (mat+ (mat3 (vec3 1)) (mat3 (vec3 2))) (mat3 (vec3 3))))

  (test-case "mat-"
    (check equal? (mat- (mat3 0)) (mat3  0))
    (check equal? (mat- (mat3 1)) (mat3 -1))
    (check equal? (mat- (mat3 (vec3 1)) 1) (mat3 (vec3  0)))
    (check equal? (mat- (mat3 (vec3 1)) (vec3 1)) (mat3 (vec3  0)))
    (check equal? (mat- (mat3 (vec3 1)) 1 (vec3 1)) (mat3 (vec3 -1)))
    (check equal? (mat- (mat3 (vec3 1)) (vec3 2)) (mat3 (vec3 -1))))

  (test-case "mat++"
    (define m (mat3 (mat3 (vec3 0)))) (check equal? m (mat3 (vec3 0)))
    (mat++ m) (check equal? m (mat3 (vec3 1)))
    (mat++ m) (check equal? m (mat3 (vec3 2))))

  (test-case "mat--"
    (define m (mat3 (mat3 (vec3 0)))) (check equal? m (mat3 (vec3 0)))
    (mat-- m) (check equal? m (mat3 (vec3 -1)))
    (mat-- m) (check equal? m (mat3 (vec3 -2))))

  ;; ...........................................................................

  (set! m (mat4 0.0 0.1 0.2 0.3
                1.0 1.1 1.2 1.3
                2.0 2.1 2.2 2.3
                3.0 3.1 3.2 3.3))

  (test-case "column-major order (4)"
    (check equal? (mat-columns m)
           (list (vec4 0.0 0.1 0.2 0.3)
                 (vec4 1.0 1.1 1.2 1.3)
                 (vec4 2.0 2.1 2.2 2.3)
                 (vec4 3.0 3.1 3.2 3.3))))

  (test-case "equal?"
    (check equal? (mat4 0) (mat4 0))
    (check equal? (mat4 1) (mat4 1))
    (check equal? (mat4) (mat4))
    (check (negate equal?) (mat4 1) (mat4 0))
    (check (negate equal?) (mat4 0) (mat4 1))
    (check (negate equal?) (mat4 0) (mat4)))

  (test-case "mac+="
    (define m
      (mat4 (mat4 0))) (check equal? m (mat4 0))
    (mat+= m (mat4 1)) (check equal? m (mat4 1))
    (mat+= m (mat4 1)) (check equal? m (mat4 2)))

  (test-case "mat-="
    (define m
      (mat4 (mat4 1))) (check equal? m (mat4 1))
    (mat-= m (mat4 1)) (check equal? m (mat4 0))
    (mat-= m (mat4 1)) (check equal? m (mat4 -1)))

  (test-case "mat+"
    (check equal? (mat4 (vec4 0)) (mat+ (mat4 (vec4 0))))
    (check equal? (mat4 (vec4 1)) (mat+ (mat4 (vec4 1))))
    (check equal? (mat4 (vec4 2)) (mat+ (mat4 (vec4 1)) 1))
    (check equal? (mat4 (vec4 2)) (mat+ (mat4 (vec4 1)) (mat4 (vec4 1))))
    (check equal? (mat4 (vec4 3)) (mat+ (mat4 (vec4 1)) 1 (mat4 (vec4 1))))
    (check equal? (mat4 (vec4 3)) (mat+ (mat4 (vec4 1)) (mat4 (vec4 2)))))

  (test-case "mat-"
    (check equal? (mat4 (vec4 0)) (mat- (mat4 (vec4 0))))
    (check equal? (mat4 (vec4 -1)) (mat- (mat4 (vec4 1))))
    (check equal? (mat4 (vec4 0)) (mat- (mat4 (vec4 1)) 1))
    (check equal? (mat4 (vec4 0)) (mat- (mat4 (vec4 1)) (vec4 1)))
    (check equal? (mat4 (vec4 -1)) (mat- (mat4 (vec4 1)) 1 (vec4 1)))
    (check equal? (mat4 (vec4 -1)) (mat- (mat4 (vec4 1)) (vec4 2))))

  ;; (test-case "mat/"
  ;;   (check equal? (mat4 1) (mat/ (mat4 1)))
  ;;   (check equal? (mat4 2) (mat/ (mat4 6) 3.0))
  ;;   (check equal? (mat4 2) (mat/ (mat4 6) (vec4 3)))
  ;;   (check equal? (mat4 1) (mat/ (mat4 6) 3.0 (vec4 2))))

  (test-case "mat++"
    (define m (mat4 (mat4 0)))
    (check equal? m (mat4 0))
    (mat++ m) (check equal? m (mat4 (vec4 1)))
    (mat++ m) (check equal? m (mat4 (vec4 2))))

  (test-case "mat--"
    (define m (mat4 (mat4 0)))
    (check equal? m (mat4 0))
    (mat-- m) (check equal? m (mat4 (vec4 -1)))
    (mat-- m) (check equal? m (mat4 (vec4 -2))))

  ;; ...........................................................................

  (test-case "mat*="
    (define m
      (mat4 (vec4 1))) (check equal? m (mat4 (vec4 1)))
    (mat*= m (mat4 (vec4 1))) (check equal? m (mat4 (vec4 4)))
    (mat*= m (mat4 (vec4 1))) (check equal? m (mat4 (vec4 16)))
    (mat*= m (mat4 (vec4 1))) (check equal? m (mat4 (vec4 64))))

  (test-case "mat*"
    (check equal? (mat* (mat4 (vec4 0))) (mat4 (vec4 0)))
    (check equal? (mat* (mat4 (vec4 0)) 0) (mat4 (vec4 0)))
    (check equal? (mat* (mat4 (vec4 0)) (vec4 0)) (vec4 0))
    (check equal? (mat* (mat4 (vec4 0)) (mat4 (vec4 0))) (mat4 (vec4 0)))
    (check equal? (mat* (mat4 (vec4 2)) 2) (mat4 (vec4 4)))
    (check equal? (mat* (mat4 (vec4 2)) (vec4 3)) (vec4 24))
    (check equal? (mat* (mat4 (vec4 2)) (mat4 3) (vec4 4)) (vec4 96))
    (check equal? (mat* (vec4 (vec4 1)) 2 (vec4 3)) (vec4 6))
    (define m (mat4 1  2  3  4
                    5  6  7  8
                    9 10 11 12
                   13 14 15 16))
    (check equal? (mat* m (vec4 1 0 0 0)) (vec4 1 2 3 4))
    (check equal? (mat* m (vec4 0 1 0 0)) (vec4 5 6 7 8))
    (check equal? (mat* m (vec4 0 0 1 0)) (vec4 9 10 11 12))
    (check equal? (mat* m (vec4 0 0 0 1)) (vec4 13 14 15 16))
    (check equal? (mat* (vec4 1 0 0 0) m) (vec4 1 5  9 13))
    (check equal? (mat* (vec4 0 1 0 0) m) (vec4 2 6 10 14))
    (check equal? (mat* (vec4 0 0 1 0) m) (vec4 3 7 11 15))
    (check equal? (mat* (vec4 0 0 0 1) m) (vec4 4 8 12 16))))
