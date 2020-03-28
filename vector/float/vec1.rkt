#lang racket/base

(require glm/scalar racket/flonum template)

(provide (all-defined-out))

(require-template glm/vector/template/vec1 define-vec1)

(define-vec1 || fl+ fl- fl* fl/ fl=)
