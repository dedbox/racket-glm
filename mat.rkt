#lang racket/base

;;; ----------------------------------------------------------------------------
;;; Matrix Base

(provide (all-defined-out))

(require ffi/unsafe
         ffi/vector
         glm/vec
         glm/vec3
         glm/vec4
         racket/flonum
         racket/function
         racket/generic
         racket/pretty
         racket/sequence)

(define (_mat rows cols) (_array (_vec cols) rows))

(define-generics glm-mat
  (mat-name glm-mat)
  (mat-num-cols glm-mat)
  (mat-num-rows glm-mat)
  (mat-predicate glm-mat)
  (mat-constructor glm-mat)
  (mat-col-predicate glm-mat)
  (mat-row-predicate glm-mat)
  (mat-col-constructor glm-mat)
  (mat-row-constructor glm-mat)
  (mat-make-col-vec glm-mat))

(struct mat (data)
  #:name glm:mat
  #:constructor-name make-mat
  #:methods gen:equal+hash
  [(define/generic gen-hash-proc hash-proc)
   (define/generic gen-hash2-proc hash2-proc)
   (define (equal-proc m1 m2 _)
     (and (= (mat-num-cols m1) (mat-num-cols m2))
          (= (mat-num-rows m1) (mat-num-rows m2))
          (andmap fl= (mat->list m1) (mat->list m2))))
   (define (hash-proc m _) (gen-hash-proc (mat-data m)))
   (define (hash2-proc m _) (gen-hash2-proc (mat-data m)))]
  #:methods gen:custom-write
  [(define (write-proc m port mode)
     (case mode
       [(#t #f) (fprintf port "#<~a>" (mat-name m))]
       [(1 0) ((if (pretty-printing) pretty-print print)
               `(,(mat-name m) ,@(mat-columns m)) port mode)]))])

(define (make-mat-data rows cols . xs)
  (define data (ptr-ref (malloc (_mat rows cols) 'atomic) (_mat rows cols) 0))
  (for ([k (in-naturals)]
        [x (in-list xs)])
    (define-values (row col) (quotient/remainder k cols))
    (array-set! data row col x))
  data)

(define (mat-copy m) 
  (apply (mat-constructor m) (mat->list m)))

(define (mat-length m)
  (* (mat-num-cols m)
     (mat-num-rows m)))

(define mat-ref
  (case-lambda
    [(m k) (let-values ([(row col) (quotient/remainder k (mat-num-cols m))])
             (mat-ref m row col))]
    [(m row col) (array-ref (mat-data m) row col)]))

(define (mat-columns m)
  (for/list ([a (in-array (mat-data m))])
    ((mat-make-col-vec m) a)))

(define (mat-rows m)
  (define vs (mat-columns m))
  (for/list ([j (in-range (mat-num-cols m))])
    (apply (mat-row-constructor m) (map (curryr vec-ref j) vs))))

(define (mat-set! m row col x)
  (array-set! (mat-data m) row col x))

(define (mat-set-column! m col v)
  (define vk (list-ref (mat-columns m) col))
  (for ([x (in-vec v)]
        [row (in-naturals)])
    (vec-set! vk row x)))

(define (mat->list m)
  (apply append (for/list ([v (in-array (mat-data m))])
                  (sequence->list (in-array v)))))

(define (mat->f32vector m)
  (cast (array-ptr (mat-data m)) _pointer (_f32vector o (mat-length m))))

(define (in-mat-columns m)
  (in-list (mat-columns m)))

(define (in-mat-rows m)
  (in-list (mat-rows m)))

(define (in-mat-cells m)
  (in-list (mat->list m)))

(define (mat=! m1 m2)
  (unless ((mat-predicate m1) m2)
    (error 'mat=! "matrices aren't the same shape"))
  (memcpy (array-ptr (mat-data m1))
          (array-ptr (mat-data m2))
          (mat-length m1)
          _float))

(define ((make-mat-binop op x0) . args)
  (define (mat-mat m1 m2)
    (apply (mat-constructor m1)
           (for/list ([v1 (in-mat-columns m1)]
                      [v2 (in-mat-columns m2)])
             (op v1 v2))))
  (define (mat-vec m v)
    (apply (mat-constructor m) (for/list ([vk (in-mat-columns m)]) (op vk v))))
  (define (vec-mat v m)
    (apply (mat-constructor m) (for/list ([vk (in-mat-columns m)]) (op v vk))))
  (define mat-binop
    (case-lambda
      [(m) (let ([m0 ((mat-constructor m) ((mat-col-constructor m) x0))])
             (mat-binop m0 m))]
      [(a b) (cond [(and (mat? a) (mat? b)) (mat-mat a b)]
                   [(mat? a) (mat-vec a b)]
                   [(mat? b) (vec-mat a b)]
                   [else (op a b)])]
      [(m . as) (for/fold ([n (mat-copy m)])
                          ([a (in-list as)])
                  (mat-binop n a))]))
  (apply mat-binop args))

(define mat+ (make-mat-binop vec+ 0.0))
(define mat- (make-mat-binop vec- 0.0))

(define (mat+= m . args) (mat=! m (apply mat+ m args)))
(define (mat-= m . args) (mat=! m (apply mat- m args)))

(define (mat++ m) (for-each vec++ (mat-columns m)))
(define (mat-- m) (for-each vec-- (mat-columns m)))
