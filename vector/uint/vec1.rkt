#lang racket/base

(require glm/scalar template)

(provide (all-defined-out))

(require-template glm/vector/template/vec1 define-vec1)

(define-vec1 u + - * / =)
