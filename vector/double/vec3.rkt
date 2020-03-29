#lang racket/base

(require glm/private/vector-ops glm/scalar racket/flonum template)

(provide (all-defined-out))

(require-template glm/vector/template/vec3 define-vec3)

(define-vec3 d fl+ fl- fl* fl/ fl=)
