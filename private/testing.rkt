#lang racket/base

(require glm/private/reprovide
         rackunit
         rackunit/text-ui
         (for-syntax racket/base))

(reprovide rackunit)

(provide (all-defined-out))

(define the-tests (make-parameter #f))

(define (run-all-tests)
  (run-tests (the-tests))
  (void))

(define-syntax-rule (define-tests name body ...)
  (the-tests (test-suite name body ...)))
