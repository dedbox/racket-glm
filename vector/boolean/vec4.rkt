#lang racket/base

(require glm/private/vector-ops glm/scalar template)

(provide (all-defined-out))

(require-templates
 [glm/vector/template/vec4    define-vec4]
 [glm/vector/template/logical define-logical-ops])

(define-vec4 b + - * / =)
(define-logical-ops 4)
