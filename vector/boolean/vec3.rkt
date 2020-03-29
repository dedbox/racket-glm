#lang racket/base

(require glm/private/vector-ops glm/scalar template)

(provide (all-defined-out))

(require-templates
 [glm/vector/template/vec3    define-vec3]
 [glm/vector/template/logical define-logical-ops])

(define-vec3 b + - * / =)
(define-logical-ops 3)
