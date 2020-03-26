#lang template ($ N)

(require glm/scalar template (for-syntax racket/base))

(provide (all-defined-out))

(for/template ([Part '(constructors accessors
                                    ;; mutators
                                    ;; updaters arithmetic bitwise-ops logical-ops
                       )])
  (require-template glm/vector/template/Part define-Part)
  (define-Part $ N))
