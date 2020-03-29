#lang template (N)

(require glm/scalar
         racket/contract
         template
         (for-syntax racket/base))

(define/contract (bvecN-and v1 v2) (-> bvecN? bvecN? bvecN?)
  (bvecN (for/template ([X (in-list '(x y z w))]
                        [_ (in-range N)])
           (bscalar (and (= (bvecN-X v1) 1) (= (bvecN-X v2) 1))))))

(define/contract (bvecN-or v1 v2) (-> bvecN? bvecN? bvecN?)
  (bvecN (for/template ([X (in-list '(x y z w))]
                        [_ (in-range N)])
           (bscalar (or (= (bvecN-X v1) 1) (= (bvecN-X v2) 1))))))
