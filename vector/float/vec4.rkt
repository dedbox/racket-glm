#lang racket/base

(require glm/scalar racket/flonum template)

(provide (all-defined-out))

(require-template glm/vector/template/vec4 define-vec4)

(define-vec4 || fl+ fl- fl* fl/ fl=)
