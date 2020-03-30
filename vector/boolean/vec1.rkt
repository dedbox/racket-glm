#lang racket/base

(require glm/private/vector-ops glm/private/vector-types glm/scalar template)

(provide (all-defined-out) bvec1?)

(require-templates
 [glm/vector/template/vec1    define-vec1]
 [glm/vector/template/logical define-logical-ops])

(define-vec1 b + - * / =)
(define-logical-ops 1)
