#lang racket/base

(require glm/private/vector-types
         glm/scalar
         template
         (for-syntax racket/base))

(provide (all-defined-out))

(define (arithmetic-rshift n m)
  (arithmetic-shift n (- m)))

(for/template ([$ (in-list '(b d || i u))])
  (for/template ([Name (in-list '(mod and or xor lshift rshift))]
                 [Op (in-list '(remainder bitwise-and bitwise-ior bitwise-xor
                                          arithmetic-shift arithmetic-rshift))])
    (define ($Name* a b) ($scalar (Op ($integer a) ($integer b)))))
  (define ($not* a) ($scalar (bitwise-not ($integer a)))))