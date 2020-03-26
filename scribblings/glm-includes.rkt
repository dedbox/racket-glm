#lang racket/base

(require racket/sandbox
         scribble/example
         scribble/manual)

(provide (all-defined-out))

(define (rtech . args)
  (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") args))

(define (gtech . args)
  (apply tech #:doc '(lib "scribblings/guide/guide.scrbl") args))

(define glm-evaluator
  (call-with-trusted-sandbox-configuration
   (λ ()
     (parameterize ([sandbox-output       'string]
                    [sandbox-error-output 'string])
       (make-base-eval #:lang 'racket/base '(void)))))) 

(define-syntax-rule (example expr ...)
  (examples #:eval glm-evaluator
            #:label #f
            #:preserve-source-locations
            expr ...))

(define-syntax-rule (EXAMPLE expr ...)
  (examples #:eval glm-evaluator
            #:escape UNSYNTAX
            #:label #f
            #:preserve-source-locations
            expr ...))
