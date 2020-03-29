#lang scribble/manual

@title[#:style 'toc]{Vector Types}

@require[
  "./glm-core-vector-type-accessors.scrbl"
  "./glm-core-vector-type-additional.scrbl"
  "./glm-core-vector-type-constructors.scrbl"
  "./glm-core-vector-type-destructive.scrbl"
  "./glm-core-vector-type-functional.scrbl"
  "./glm-core-vector-type-mutators.scrbl"
  template
  @for-syntax[racket/base]
  @for-label[glm racket/base racket/contract]]

@for/template[([N '(1 2 3 4)])
  @define[N-components @list["N " @if-template[(= N 1) "component" "components"]]]
  @define[N-components. @list[@N-components "."]]]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@local-table-of-contents[]

@defmodule[glm/vector]

@for/template[([N '(1 2 3 4)])
  @defproc[(tvecN? [a any/c]) boolean?]{

    Returns @racket[#t] if @var[a] is a vector with @N-components, @racket[#f]
    otherwise.

  }

  @for/template[([X '(x y z w)]
                 [R_ '(r g b a)]
                 [S '(s t p q)]
                 [I-th '(first second third fourth)]
                 [_ N])]{

    @deftogether[(
    @defproc[(tvecN-X [v tvecN?]) tscalar?]
    @defproc[(tvecN-R_ [v tvecN?]) tscalar?]
    @defproc[(tvecN-S [v tvecN?]) tscalar?]
    )]{

      Returns the I-th component of @var[v].

    }
  }

  @for/template[([X '(x y z w)]
                 [R '(r g b a)]
                 [S_ '(s t p q)]
                 [I-th '(first second third fourth)]
                 [_ N])]{

    @deftogether[(
    @defproc[(set-tvecN-X! [v tvecN?] [a tscalar?]) void?]
    @defproc[(set-tvecN-R! [v tvecN?] [a tscalar?]) void?]
    @defproc[(set-tvecN-S_! [v tvecN?] [a tscalar?]) void?]
    )]{

      Sets the I-th component of @var[v] to @var[a].

    }
  }
]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@for/template[([Type '(boolean double float int uint)]
               [$ '(b d || i u)]
               [Name '(Boolean |Double-Precision Floating-Point|
                               |Single-Precision Floating-Point|
                               |Signed Integer|
                               |Unsigned Integer|)])]{
  @section[#:style '(quiet toc)]{Name Vectors}

  @defmodule[glm/vector/Type]

  @local-table-of-contents[]

  @for/template[([Length '(One Two Three Four)]
                 [N '(1 2 3 4)])]{
    @subsection[#:tag "$vecN-vectors"]{Length-Element Name Vectors}

    @defmodule[glm/vector/Type/vecN #:no-declare]

    @document-vector-type-constructors[Type $ N]
    @document-vector-type-accessors[Type $ N]
    @document-vector-type-mutators[Type $ N]
    @document-vector-type-destructive[Type $ N]
    @document-vector-type-functional[Type $ N]
    @document-vector-type-additional[Type $ N]
  }
}
