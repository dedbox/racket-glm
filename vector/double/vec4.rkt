#lang racket/base

(require glm/private/vector-ops glm/scalar racket/flonum template)

(provide (all-defined-out))

(require-template glm/vector/template/vec4 define-vec4)

(define-vec4 d fl+ fl- fl* fl/ fl=)
