#lang racket/base

(require glm/private/vector-ops glm/private/vector-types glm/scalar template)

(provide (all-defined-out) bvec2?)

(require-templates
 [glm/vector/template/vec2    define-vec2]
 [glm/vector/template/logical define-logical-ops])

(define-vec2 b + - * / =)
(define-logical-ops 2)
