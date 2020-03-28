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
  @for-label[racket/base racket/contract]]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@local-table-of-contents[]

@defmodule[glm/vector]


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
