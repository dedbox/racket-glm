#lang racket/base

(require glm/private/types
         glm/scalar
         glm/vec1
         glm/vec2
         glm/vec3
         glm/vec4
         racket/contract
         template
         (for-syntax racket/base))

(provide (all-defined-out)
         tvec? tvec1? tvec2? tvec3? tvec4? tvec-length)

(for/template ([$ (in-list '(b d || i u))])
  (define/contract $vec? predicate/c (or/c $vec1? $vec2? $vec3? $vec4?)))
