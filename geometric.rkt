#lang racket/base

;;; ----------------------------------------------------------------------------
;;; Geometric Functions
;;;
;;; glm/geometric.hpp
;;; glm/detail/func_geometric.inl

(require glm/vec
         math/flonum)

(provide (all-defined-out))

;;; "length" is taken
(define (magnitude v)
  (sqrt (dot v v)))

(define (distance p0 p1)
  (magnitude (vec- p1 p0)))

(define (dot a b)
  (flsum (vec->list (vec* a b))))

(define (cross x y)
  (define-values (x.x x.y x.z) (apply values (vec->list x)))
  (define-values (y.x y.y y.z) (apply values (vec->list y)))
  (vec3 (- (* x.y y.z) (* y.y x.z))
        (- (* x.z y.x) (* y.z x.x))
        (- (* x.x y.y) (* y.x x.y))))

(define (normalize v)
  (vec* v (/ (magnitude v))))

(define (faceforward N I Nref)
  (if (< (dot Nref I) 0.0) N (vec- N)))

(define (reflect I N)
  (vec- I (vec* N (dot N I) 2.0)))

(define (refract I N eta)
  (define dot-value (dot N I))
  (define k (- 1.0 (* eta eta (- 1.0 (* dot-value dot-value)))))
  (if (>= k 0.0)
      (vec- (vec* eta I) (vec* (+ (* eta dot-value) k) N))
      ((vec-constructor I) 0.0)))
