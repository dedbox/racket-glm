#lang racket/base

(require glm/private/vector-ops glm/scalar template)

(provide (all-defined-out))

(require-template glm/vector/template/vec2 define-vec2)

(define-vec2 i + - * / =)
