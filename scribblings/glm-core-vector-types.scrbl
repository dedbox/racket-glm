#lang scribble/manual

@title[#:style 'toc]{Vector Types}

@require[
  "./glm-core-vector-type-accessors.scrbl"
  "./glm-core-vector-type-additional.scrbl"
  "./glm-core-vector-type-constructors.scrbl"
  "./glm-core-vector-type-mutators.scrbl"
  "./glm-core-vector-type-updaters.scrbl"
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

@define-template[@document-vector-type[Type $ Name]]{
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
    @document-vector-type-updaters[Type $ N]
    @document-vector-type-additional[Type $ N]

    @subsubsection[#:tag "TypeN-Vector-Arithmetic"]{Arithmetic}

    @subsubsection[#:tag "TypeN-Vector-Bitwise-Ops"]{Bitwise Operations}

    @subsubsection[#:tag "TypeN-Vector-Logical-Ops"]{Logical Operations}
  }
}

@document-vector-type[boolean b Boolean]
@document-vector-type[double d |Double-Precision Floating-Point|]
@document-vector-type[float || |Single-Precision Floating-Point|]
@document-vector-type[int i |Signed Integer|]
@document-vector-type[uint u |Unsigned Integer|]
