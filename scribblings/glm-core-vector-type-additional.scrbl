#lang at-exp racket/base

@require[
  "./glm-includes.rkt"
  scribble/manual
  template
  @for-syntax[racket/base]
  @for-label[glm racket/contract template]]

@provide[document-vector-type-additional]

@define-template[@make-or/c[N]
  @cond-template[
    [(= N 1) 0]
    [(= N 2) (or/c 0 1)]
    [(= N 3) (or/c 0 1 2)]
    [(= N 4) (or/c 0 1 2 3)]]]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@define-template[@document-vector-type-additional[Type $ N]]{
  @defproc[($vecN-ref [v $vecN?] [i (make-or/c N)]) $scalar?]{

    Returns the @var[i]th component of @var[v].

  }

  @defproc[($vecN->list [v $vecN?]) (listof $scalar?)]{

    Returns the components of @var[v] as a list.

  }
}
