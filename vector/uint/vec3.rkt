#lang racket/base

(require glm/private/vector-ops glm/scalar template)

(provide (all-defined-out))

(require-template glm/vector/template/vec3 define-vec3)

(define-vec3 u + - * / =)
