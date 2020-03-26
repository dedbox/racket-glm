#lang at-exp racket/base

@require[
  "./glm-includes.rkt"
  scribble/manual
  template
  @for-syntax[racket/base]
  @for-label[glm racket/contract template]]

@provide[document-vector-type-mutators]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@define-template[@document-vector-type-mutators[Type $ N]]{
  @for/template[([X '(x y z w)]
                 [R '(r g b a)]
                 [S_ '(s t p q)]
                 [I-th '(first second third fourth)]
                 [_ N])]{

    @deftogether[(
    @defproc[(set-$vecN-X! [v $vecN?] [a $scalar?]) void?]
    @defproc[(set-$vecN-R! [v $vecN?] [a $scalar?]) void?]
    @defproc[(set-$vecN-S_! [v $vecN?] [a $scalar?]) void?]
    )]{

      Sets the I-th component of @var[v] to @var[a].

    }
  }
}
