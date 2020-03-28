#lang racket/base

(require glm/scalar racket/flonum template)

(provide (all-defined-out))

(require-template glm/vector/template/vec3 define-vec3)

(define-vec3 || fl+ fl- fl* fl/ fl=)
