#lang scribble/manual

@title{Scalar Types}

@require[
  "./glm-includes.rkt"
  template
  @for-syntax[racket/base]
  @for-label[racket/base racket/contract]]

@example[#:hidden @require[glm]]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@defmodule[glm/scalar]

@defproc[(tscalar? [a any/c]) boolean?]{

  Returns @racket[#t] if @var[a] is a scalar, @racket[#f] otherwise.

  Example:
  @example[
(map tscalar? '(-1 0 1 2.0 z))
  ]
}

@for/template[([$ '(b d i u)]
               [a-T '(|a boolean| |a double| |an int| |a uint|)])]{
  @defproc[($scalar? [a any/c]) boolean?]{

    Returns @racket[#t] if @var[a] is a-T scalar, @racket[#f] otherwise.

    Example:
    @example[
(map $scalar? '(-1 0 1 2.0 z))
    ]
  }

  @defproc[($scalar [a any/c]) $scalar?]{

    Returns the numeric representation of @var[a] as a-T scalar.

    Example:
    @example[
(map $scalar '(-1 0 1 2.0 z))
    ]
  }
}
