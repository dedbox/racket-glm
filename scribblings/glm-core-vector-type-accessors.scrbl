#lang at-exp racket/base

@require[
  "./glm-includes.rkt"
  scribble/manual
  template
  @for-syntax[racket/base]
  @for-label[glm racket/contract template]]

@provide[document-vector-type-accessors]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@define-template[@document-vector-type-accessors[Type $ N]]{
  @for/template[([X '(x y z w)]
                 [R_ '(r g b a)]
                 [S '(s t p q)]
                 [I-th '(first second third fourth)]
                 [_ N])]{

    @deftogether[(
    @defproc[($vecN-X [v $vecN?]) $scalar?]
    @defproc[($vecN-R_ [v $vecN?]) $scalar?]
    @defproc[($vecN-S [v $vecN?]) $scalar?]
    )]{

      Returns the I-th component of @var[v].

    }
  }
}
