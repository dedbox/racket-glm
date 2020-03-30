#lang racket/base

(require glm/private/vector-ops glm/private/vector-types glm/scalar template)

(provide (all-defined-out) ivec4?)

(require-template glm/vector/template/vec4 define-vec4)

(define-vec4 i + - * / =)
