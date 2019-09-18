#lang racket/base

(require glm/mat
         glm/mat4
         glm/vec
         glm/vec3
         glm/vec4)

(provide (all-defined-out))

(define (get-vec-constructor len)
  (cond [(= len 3) vec3]
        [(= len 4) vec4]
        [else (error 'unsupported)]))

(define (get-mat-constructor rows cols)
  (cond [(and (= rows 4) (= cols 4)) mat4]
        [else (error 'unsupported)]))
