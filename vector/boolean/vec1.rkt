#lang racket/base

(require glm/private/vector-ops glm/scalar template)

(provide (all-defined-out))

(require-templates
 [glm/vector/template/vec1    define-vec1]
 [glm/vector/template/logical define-logical-ops])

(define-vec1 b + - * / =)
(define-logical-ops 1)
