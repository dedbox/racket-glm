#lang scribble/manual

@title{OpenGL Mathematics (GLM)}
@author{@author+email["Eric Griffis" "dedbox@gmail.com"]}

@require{./glm-includes.rkt}

@require[
  template
  @for-label[
    racket/base
    (except-in racket/contract any)
    glm
  ]
  @for-syntax[
    racket/base
  ]
]

@example[#:hidden @require[glm]]

@table-of-contents[]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@section{Vector Types}

@defmodule[glm/vector]

@for/template[([$ (in-list '(b d || i u))]
               [TT (in-list '(Boolean
                              |Double-Precision Floating-Point|
                              |Single-Precision Floating-Point|
                              |Signed Integer|
                              |Unsigned Integer|))])]{

@; -----------------------------------------------------------------------------

  @subsection{TT Vectors}

  @deftogether[(
    @for/template[([N (in-list '(1 2 3 4))])]{
      @defthing[#:kind "procedure" $vecN? predicate/c]
    }
  )]

  @deftogether[(
    @for/template[([N (in-list '(1 2 3 4))])]{
      @defthing[#:kind "procedure" $vecN (-> any/c ... $vecN?)]
    }
  )]

@; .............................................................................

  @subsubsection{TT Vector Component Accessors}

  @deftogether[(
    @for/template[([N (in-list '(1 2 3 4))])]{
      @defthing[#:kind "procedure" $vecN-ref (-> $vecN? natural? $scalar?)]
    }
  )]

  @for/template[([N (in-list '(1 2 3 4))])]{
    @deftogether[(
      @for/template[([XYZW (in-list '(x y z w))]
                     [_ (in-range N)])]{
        @defthing[#:kind "procedure" $vecN-XYZW (-> $vecN? $scalar?)]
      }
    )]

    @deftogether[(
      @for/template[([RGBA (in-list '(r g b a))]
                     [_ (in-range N)])]{
        @defthing[#:kind "procedure" $vecN-RGBA (-> $vecN? $scalar?)]
      }
    )]

    @deftogether[(
      @for/template[([STPQ (in-list '(s t p q))]
                     [_ (in-range N)])]{
        @defthing[#:kind "procedure" $vecN-STPQ (-> $vecN? $scalar?)]
      }
    )]
  }

@; .............................................................................

  @subsubsection{TT Vector Component Mutators}

  @for/template[([N (in-list '(1 2 3 4))])]{
    @deftogether[(
      @for/template[([XYZW (in-list '(x y z w))]
                     [_ (in-range N)])]{
        @defthing[#:kind "procedure" set-$vecN-XYZW! (-> $vecN? $scalar? $vecN?)]
      }
    )]

    @deftogether[(
      @for/template[([RGBA (in-list '(r g b a))]
                     [_ (in-range N)])]{
        @defthing[#:kind "procedure" set-$vecN-RGBA! (-> $vecN? $scalar? $vecN?)]
      }
    )]

    @deftogether[(
      @for/template[([STPQ (in-list '(s t p q))]
                     [_ (in-range N)])]{
        @defthing[#:kind "procedure" set-$vecN-STPQ! (-> $vecN? $scalar? $vecN?)]
      }
    )]
  }

@; .............................................................................

  @subsubsection{TT Vector Assignment}

  @for/template[([N (in-list '(1 2 3 4))])]{
    @deftogether[(
      @defthing[#:kind "procedure" $vecN=! (-> $vecN? tvecN? $vecN?)]
      @defthing[#:kind "procedure" $vecN=tvecN! (-> $vecN? tvecN? $vecN?)]
      @defthing[#:kind "procedure" $vecN=$vecN! (-> $vecN? $vecN? $vecN?)]
    )]
  }

@; .............................................................................

  @subsubsection{TT Vector Mutation}

  @for/template[([N (in-list '(1 2 3 4))])]{
    @deftogether[(
      @defthing[#:kind "procedure" $vecN++! (-> $vecN? $vecN?)]
      @defthing[#:kind "procedure" $vecN--! (-> $vecN? $vecN?)]
      @for/template[([⊙ (in-list '(+ - * /))])
        @defthing[#:kind "procedure" $vecN⊙=! (-> $vecN? any/c $vecN?)]
        @unless-template[(= N 1)
          @defthing[#:kind "procedure" $vecN⊙=tvec1! (-> $vecN? tvec1? $vecN?)]]
        @defthing[#:kind "procedure" $vecN⊙=tvecN! (-> $vecN? tvecN? $vecN?)]
        @defthing[#:kind "procedure" $vecN⊙=$vecN! (-> $vecN? $vecN? $vecN?)]
        @defthing[#:kind "procedure" $vecN⊙=$scalar! (-> $vecN? $scalar? $vecN?)]
      ]
    )]
  }

@; .............................................................................

  @subsubsection{TT Vector Arithmetic}

  @for/template[([N (in-list '(1 2 3 4))])]{
    @deftogether[(
      @for/template[([⊙ (in-list '(+ - * /))])
        @defthing[#:kind "procedure" $vecN⊙ (-> $vecN? any/c $vecN?)]
        @unless-template[(= N 1)
          @defthing[#:kind "procedure" $vecN⊙tvec1 (-> $vecN? tvec1? $vecN?)]]
        @defthing[#:kind "procedure" $vecN⊙tvecN (-> $vecN? tvecN? $vecN?)]
        @defthing[#:kind "procedure" $vecN⊙$vecN (-> $vecN? $vecN? $vecN?)]
        @defthing[#:kind "procedure" $vecN⊙$scalar (-> $vecN? $scalar? $vecN?)]
      ]
    )]
  }

@; .............................................................................

  @subsubsection{TT Vector Bitwise Operations}

  @for/template[([N (in-list '(1 2 3 4))])]{
    @deftogether[(
      @for/template[([⊙ (in-list '(% & // ^ << >>))])
        @defthing[#:kind "procedure" $vecN⊙=! (-> $vecN? any/c $vecN?)]
        @unless-template[(= N 1)
          @defthing[#:kind "procedure" $vecN⊙=tvec1! (-> $vecN? tvec1? $vecN?)]]
        @defthing[#:kind "procedure" $vecN⊙=tvecN! (-> $vecN? tvecN? $vecN?)]
        @defthing[#:kind "procedure" $vecN⊙=$vecN! (-> $vecN? $vecN? $vecN?)]
        @defthing[#:kind "procedure" $vecN⊙=$scalar! (-> $vecN? $scalar? $vecN?)]
      ]
    )]
  }

  @for/template[([N (in-list '(1 2 3 4))])]{
    @deftogether[(
      @for/template[([⊙ (in-list '(% & // ^ << >>))])
        @defthing[#:kind "procedure" $vecN⊙ (-> $vecN? any/c $vecN?)]
        @unless-template[(= N 1)
          @defthing[#:kind "procedure" $vecN⊙tvec1 (-> $vecN? tvec1? $vecN?)]]
        @defthing[#:kind "procedure" $vecN⊙tvecN (-> $vecN? tvecN? $vecN?)]
        @defthing[#:kind "procedure" $vecN⊙$vecN (-> $vecN? $vecN? $vecN?)]
        @defthing[#:kind "procedure" $vecN⊙$scalar (-> $vecN? $scalar? $vecN?)]
      ]
      @defthing[#:kind "procedure" $vecN~ (-> $vecN? $vecN?)]
    )]
  }

@; .............................................................................

  @subsubsection{TT Vector Boolean Operations}

  @deftogether[(
    @for/template[([N (in-list '(1 2 3 4))])]{
      @defthing[#:kind "procedure" $vecN=? (-> $vecN? $vecN? boolean?)]
    }
  )]

  @when-template[(eq? '$ 'b)
    @deftogether[(
      @for/template[([N (in-list '(1 2 3 4))])
        @defthing[#:kind "procedure" bvecN-and (-> bvecN? bvecN? bvecN?)]]
    )]

    @deftogether[(
      @for/template[([N (in-list '(1 2 3 4))])
        @defthing[#:kind "procedure" bvecN-or (-> bvecN? bvecN? bvecN?)]]
    )]]

}

@; -----------------------------------------------------------------------------

@subsection{Vector Relational Functions}

@defmodule[glm/vector/relational]

@for/template[([OP '(lessThan lessThanEqual greaterThan greaterThanEqual
                     equal notEqual)])]{
  @deftogether[(
    @defthing[#:kind "procedure" OP (-> tvec? tvec? tvec?)]
    @for*/template[([$ (in-list '(b d || i u))]
                    [N (in-list '(1 2 3 4))])]{
      @defthing[#:kind "procedure" OP/$vecN (-> $vecN? $vecN? $vecN?)]
    }
  )]
}

@for/template[([OP '(any all)])]{
  @deftogether[(
    @defthing[#:kind "procedure" OP (-> tvec? bscalar?)]
    @for*/template[([$ (in-list '(b d || i u))]
                    [N (in-list '(1 2 3 4))])]{
      @defthing[#:kind "procedure" OP/$vecN (-> $vecN? bscalar?)]
    }
  )]
}

@begin-template[
  @deftogether[(
    @defthing[#:kind "procedure" not- (-> bvec? bvec?)]
    @for/template[([N (in-list '(1 2 3 4))])]{
      @defthing[#:kind "procedure" not-/bvecN (-> bvecN? bvecN?)]
    }
  )]
]

@; -----------------------------------------------------------------------------

@subsection{Additional Vector Type Functions}

@defmodule[glm/vector/common]

@begin-template[
  @deftogether[(
    @for/template[([N (in-list '(|| 1 2 3 4))])]{
      @defthing[#:kind "procedure" tvecN? predicate/c]
    }
  )]
]

@defthing[#:kind "procedure" tvec-length (-> tvec? natural?)]

@; @defthing[#:kind "procedure" tvec-ref (-> tvec? natural? tscalar?)]

@begin-template[
  @deftogether[(
    @for/template[([$ (in-list '(b d || i u))])]{
      @defthing[#:kind "procedure" $vec? predicate/c]
    }
  )]
]

@; =============================================================================

@section{Scalar Types}

@defmodule[glm/scalar]

@begin-template[
  @deftogether[(
    @for/template[([$ (in-list '(t b d || i u))])]{
      @defthing[#:kind "procedure" $scalar? predicate/c]
    }
  )]

  @deftogether[(
    @for/template[([$ (in-list '(b d || i u))])]{
      @defthing[#:kind "procedure" $scalar (-> any/c $scalar?)]
    }
  )]
]

@defproc[(bool [x bscalar?]) boolean?]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@require[scribble/example]

@close-eval[glm-evaluator]
