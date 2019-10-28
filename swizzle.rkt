#lang racket/base

(require glm/vec
         racket/function
         syntax/parse/define
         (for-syntax glm/swizzle-syntax
                     racket/base
                     racket/function
                     racket/list
                     racket/string
                     racket/syntax))

(provide (rename-out [swizzle-dot #%dot]
                     [swizzle-top-interaction #%top-interaction]))

(begin-for-syntax
  (define-syntax-class swizzled-id
    #:attributes (a b)
    (pattern :id
             #:do [(define sym (syntax-e this-syntax))
                   (define str (symbol->string sym))]
             #:when (and (string-contains? str ".")
                         (not (char=? (string-ref str 0) #\.)))
             #:with (a b)
             (let*-values ([(strs) (string-split str ".")]
                           [(a-strs b-strs) (split-at-right strs 1)])
               (define a-str (string-join a-strs "."))
               (define b-str (car b-strs))
               (list (datum->syntax this-syntax (string->symbol a-str))
                     (datum->syntax this-syntax (string->symbol b-str)))))))

(define-for-syntax (swizzle-parse-top stx)
  ;; (println `(TOP ,(syntax->datum stx)))
  (syntax-parse stx
    #:literals (#%expression module begin begin-for-syntax)
    [(#%expression expr) (swizzle-expand (attribute expr))]
    [((~literal module) id path (#%plain-module-begin form ...))
     #:with (form* ...) (map swizzle-expand-module (attribute form))
     #'(module id path (mod-begin form* ...))]
    [((~and begin head) expr ...)
     #:with (expr* ...) (map swizzle-expand-top (attribute expr))
     #'(head expr* ...)]
    [((~and begin-for-syntax head) form ...)
     #:with (form* ...) (map swizzle-expand-top (attribute form))
     #'(head form* ...)]
    [_ (swizzle-expand-general this-syntax)]))

(define-for-syntax (swizzle-parse-module stx)
  ;; (println `(MODULE ,(syntax->datum stx)))
  (syntax-parse stx
    #:literal-sets (kernel-literals)
    [(#%provide . _) this-syntax]
    [((~and begin-for-syntax head) form ...)
     #:with (form* ...) (map swizzle-expand-top-module (attribute form))
     #'(head form* ...)]
    [(#%provide . _) this-syntax]
    [((~and module head) id path (#%plain-module-begin form ...))
     #:with (form* ...) (map swizzle-expand-top-module (attribute form))
     #'(head id path form* ...)]
    [((~and module* head) id path (#%plain-module-begin form ...))
     #:with (form* ...) (map swizzle-expand-top-module (attribute form))
     #'(head id path form* ...)]
    [_ (swizzle-expand-general this-syntax)]))

(define-for-syntax (swizzle-parse-top-module stx)
  ;; (println `(TOP-MODULE ,(syntax->datum stx)))
  (syntax-parse stx
    #:literal-sets (kernel-literals)
    [((~and module head) id path (#%plain-module-begin form ...))
     #:with (form* ...) (map swizzle-expand-module (attribute form))
     #'(head id path form* ...)]
    [((~and module* head) id path (#%plain-module-begin form ...))
     #:with (form* ...) (map swizzle-expand-module (attribute form))
     #'(head id path form* ...)]
    [_ (swizzle-expand-module this-syntax)]))

(define-for-syntax (swizzle-parse-general stx)
  ;; (println `(GENERAL ,(syntax->datum stx)))
  (syntax-parse stx
    #:literal-sets (kernel-literals)
    [((~and define-values head) (x ...) expr)
     #:with expr* (swizzle-expand (attribute expr))
     #'(head (x ...) expr*)]
    [((~and define-syntaxes head) (x ...) expr)
     #:with expr* (swizzle-expand (attribute expr))
     #'(head (x ...) expr*)]
    [(#%require . _) this-syntax]
    [_ (swizzle-expand this-syntax)]))

(define-for-syntax (swizzle-parse-expr stx)
  ;; (println `(EXPR ,(syntax->datum stx)))
  (syntax-parse stx
    #:literal-sets (kernel-literals)
    [x:swizzled-id
     #:with #%dot (format-id #'x "#%dot")
     #:with a* (swizzle-expand (attribute x.a))
     #'(#%dot a* x.b)]
    [:id this-syntax]
    [(#%plain-lambda formals expr ...)
     #:with (expr* ...) (map swizzle-expand (attribute expr))
     #'(#%plain-lambda formals expr* ...)]
    [(case-lambda (formals expr ...) ...)
     #:with ((expr* ...) ...) (map (curry map swizzle-expand) (attribute expr))
     #'(case-lambda (formals expr* ...) ...)]
    [(if e1 e2 e3)
     #:with e1* (swizzle-expand (attribute e1))
     #:with e2* (swizzle-expand (attribute e2))
     #:with e3* (swizzle-expand (attribute e3))
     #'(if e1* e2* e3*)]
    [((~and begin head) expr ...)
     #:with (expr* ...) (map swizzle-expand (attribute expr))
     #'(head expr* ...)]
    [((~and begin0 head) expr ...)
     #:with (expr* ...) (map swizzle-expand (attribute expr))
     #'(head expr* ...)]
    [((~and let-values head) ([(x ...) expr] ...) body ...)
     #:with (expr* ...) (map swizzle-expand (attribute expr))
     #:with (body* ...) (map swizzle-expand (attribute body))
     #'(head ([(x ...) expr*] ...) body* ...)]
    [((~and letrec-values head) ([(x ...) expr] ...) body ...)
     #:with (expr* ...) (map swizzle-expand (attribute expr))
     #:with (body* ...) (map swizzle-expand (attribute body))
     #'(head ([(x ...) expr*] ...) body* ...)]
    ;; [(set! )]
    [(set! x expr)
     #:with expr* (swizzle-expand (attribute expr))
     #'(set! x expr*)]
    [(quote _) this-syntax]
    [(quote-syntax . _) this-syntax]
    [((~and with-continuation-mark head) e1 e2 e3)
     #:with e1* (swizzle-expand (attribute e1))
     #:with e2* (swizzle-expand (attribute e2))
     #:with e3* (swizzle-expand (attribute e3))
     #'(head e1* e2* e3*)]
    [((~and #%plain-app head) expr ...)
     #:with (expr* ...) (map swizzle-expand (attribute expr))
     #'(head expr* ...)]
    [(#%top . _) this-syntax]
    [(#%variable-reference . _) this-syntax]

    ;; [(expr ...)
    ;;  #:with (expr* ...) (map swizzle-expand (attribute expr))
    ;;  #'(expr* ...)]

    ))

(define-for-syntax (swizzle-expand-top stx)
  (swizzle-parse-top (local-expand stx 'top-level #f)))

(define-for-syntax (swizzle-expand-top-module stx)
  (swizzle-parse-top-module (local-expand stx 'top-level #f)))

(define-for-syntax (swizzle-expand-module stx)
  (swizzle-parse-module (local-expand stx 'module #f)))

(define-for-syntax (swizzle-expand-general stx)
  (swizzle-parse-general (local-expand stx 'module #f)))

(define-for-syntax (swizzle-expand stx)
  (swizzle-parse-expr (local-expand stx 'expression null)))

(define-syntax-parser swizzle-dot
  [(dot a:expr b:swizzle-id) #'(swizzle a b)]
  [(dot a b:id)
   (define a* (local-expand #'a 'expression null))
   (if (identifier? a*)
       (format-id #'dot "~a.~a" a* #'b)
       (raise-syntax-error #f "invalid swizzle" this-syntax))])

(define-simple-macro (swizzle-top-interaction . form)
  #:with form* (swizzle-expand-top #'form)
  (#%top-interaction . form*))

(define (component-index sym)
  (case sym
    [(x r s) 0]
    [(y g t) 1]
    [(z b p) 2]
    [(w a q) 3]))

(define (component-indices sym)
  (map (compose component-index string->symbol string)
       (string->list (symbol->string sym))))

(define-simple-macro (swizzle vec-expr:expr components:swizzle-id)
  (let ([v vec-expr])
    (define num-components (string-length (symbol->string 'components)))
    (if (= num-components 1)
        (vec-ref v (component-index 'components))
        (apply vec #:length num-components
               (map (curry vec-ref v) (component-indices 'components))))))

(module reader racket/base
  (require racket/port)

  (provide (rename-out
            [swizzle-read read]
            [swizzle-read-syntax read-syntax]))

  (define (swizzle-read port)
    (parameterize ([read-cdot #t])
      `(module ,(gensym 'swizzle) ,(read port)
         (require glm glm/swizzle)
         ,@(port->list read port))))

  (define (swizzle-read-syntax path port)
    (datum->syntax #f (swizzle-read port))))
