#lang template ($ N ⊕ ⊖ ⊗ ⊘ ≗)

(require glm/scalar template (for-syntax racket/base))

(provide (all-defined-out))

(require-templates
 [glm/vector/template/constructors define-constructors]
 [glm/vector/template/accessors    define-accessors   ]
 [glm/vector/template/mutators     define-mutators    ]
 [glm/vector/template/updaters     define-updaters    ]
 ;; [glm/vector/template/arithmetic   define-arithmetic-ops]
 ;; [glm/vector/template/bitwise      define-bitwise-ops   ]
 ;; [glm/vector/template/logical      define-logical-ops   ]
 [glm/vector/template/additional   define-additional-ops])

(define-constructors $ N)
(define-accessors $ N)
(define-mutators $ N)
(define-updaters $ N ⊕ ⊖ ⊗ ⊘)
;; (define-arithmetic $ N ⊕ ⊖ ⊗ ⊘ ≗)
;; (define-bitwise-ops $ N ⊕ ⊖ ⊗ ⊘ ≗)
;; (define-logical-ops $ N ⊕ ⊖ ⊗ ⊘ ≗)
(define-additional-ops $ N)
