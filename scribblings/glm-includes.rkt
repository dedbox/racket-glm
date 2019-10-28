#lang racket/base

(require racket/sandbox
         racket/string
         scribble/examples
         scribble/manual)

(provide (all-defined-out))

;; (random-seed 7)

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
  (examples #:eval glm-evaluator #:label #f expr ...))

(void (example #:hidden (require ffi/unsafe
                                 ffi/vector
                                 glm
                                 racket/match
                                 racket/sequence)))

(define-syntax deflink
  (syntax-rules ()
    [(_ name content url) (define name (hyperlink url content))]
    [(_ name url)
     (deflink name
       (for/fold ([str (format "~a" 'name)])
                 ([from (in-string "-<>")]
                  [to   (in-string " ()")])
         (string-replace str (string from) (string to)))
       url)]))

;;; ----------------------------------------------------------------------------
;;; Technical Terms

(define component (tech "component"))
(define component. (list component "."))

(define components (tech "components"))
(define components. (list components "."))

(define Matrices (tech #:key "matrix" "Matrices"))

(define matrices (tech #:key "matrix" "matrices"))
(define matrices. (list matrices "."))

(define matrix (tech "matrix"))
(define matrix. (list matrix "."))

(define vector (tech "vector"))
(define vector. (list vector "."))

(define vectors (tech "vectors"))
(define vectors. (list vectors "."))

(define Vectors (tech "Vectors"))
