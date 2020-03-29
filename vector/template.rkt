#lang template ($ N ⊕ ⊖ ⊗ ⊘ ≗)

(require glm/scalar template (for-syntax racket/base))

(provide (all-defined-out))

(require-templates
 [glm/vector/template/constructors define-constructors]
 [glm/vector/template/accessors    define-accessors   ]
 [glm/vector/template/mutators     define-mutators    ]
 [glm/vector/template/destructive define-destructive-ops]
 [glm/vector/template/functional  define-functional-ops ]
 [glm/vector/template/additional  define-additional-ops ])

(define-constructors $ N)
(define-accessors    $ N)
(define-mutators     $ N)
(define-destructive-ops $ N ⊕ ⊖ ⊗ ⊘)
(define-functional-ops  $ N ⊕ ⊖ ⊗ ⊘ ≗)
(define-additional-ops  $ N)
