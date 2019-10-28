#lang scribble/manual

@title{Vector Types}

@require{./glm-includes.rkt}

@require[
  racket/string
  scribble/example
  @for-label[
    (except-in ffi/unsafe ->)
    ffi/vector
    glm
    racket/base
    racket/contract/base
    racket/match
    racket/math
    racket/sequence
  ]
]

@; #############################################################################

@defmodule[glm/vector-types]

This module re-exports @racketmodname[glm/bvec], @racketmodname[glm/dvec],
@racketmodname[glm/ivec], @racketmodname[glm/uvec], and
@racketmodname[glm/vec].

@; =============================================================================

@section[#:tag "vector:Single_Precision_Floats"]{Single Precision Floats}

@defmodule[glm/vec]

A @deftech{vector} is an array of 32-bit floating point @deftech{component}
values.

Two @vectors are @racket[equal?] iff they have the same number of @components
and each pair of consecutive @components are @racket[=].

@defproc[(vec? [v any/c]) boolean?]{

  Returns @racket[#t] if @var[v] is a @vector.

}

@defproc[
  (vec [#:length len (or/c exact-positive-integer? #f) #f]
       [#:fill fill (or/c real? 'no-fill) 0]
       [x (or/c vec? real?)] ...) vec?
]{

  Allocates a @vector with the @var[x]s as its @components.

  @example[
    (vec 1 2/3 4.5 -6.7e8)
  ]

  The @components of any @vectors in the @var[x]s are spliced into the final
  @component list.

  @example[
    (vec (vec 1 2) 3 4 (vec 5 6 (vec 7 8)) 9)
  ]

  When both @var[len] and @var[fill] are numbers, the final @component list
  can have any number of values. If its length exceeds @var[len], the excess
  values are truncated; otherwise, any missing values default to @var[fill].

  @example[
    (vec #:length 5 1 2 3)
    (vec #:length 5 1 2 3 4 5 6 7)
  ]

  When only @var[len] is a number, the final @component list must have exactly
  @var[len] values. Giving more or less than @var[len] values is an error.

  @example[
    (eval:error (vec #:length 3 #:fill 'no-fill (vec 1 2) (vec 3 4)))
  ]

  @example[
    (eval:error (vec #:length 3 #:fill 'no-fill))
  ]

  If @var[len] ≥ 2 and only one @component value is given, every value
  defaults to it.

  @example[
    (vec #:length 3 7)
  ]

  @racket[vec] is also a @racket[match] pattern identifier for deconstructing
  @vectors.

  @example[
    (match (vec 1 2 3)
      [(vec a b c) (+ a b c)])
  ]

  @Vectors of indeterminate length can be matched with a final @racket[#:rest]
  pattern.

  @example[
    (match (vec 1 2 3 4 5)
      [(vec x y z #:rest tail) (list x y z (apply + tail))])
  ]
}

@defproc[
  (make-vec [data array?]
            [len exact-positive-integer?]
            [fixed? boolean?]
            [fill (or/c real? 'no-fill)])
  vec?
]{

  Constructs a fresh @vector on the first @var[len] @components of some
  existing @var[data]. The array is not copied; the new Racket representation
  is backed by the existing C representation.

  If @var[fixed?] is true and @var[len] is between 0 and 4, the @vector
  displays its length-suffixed name. Otherwise, it uses the more general
  @racket[(vec ...)] form.

  Example:
  @example[
    (define v1 (vec #:length 5 1))
    (define v2 (make-vec (vec-data v1) 3 #t 0))
    v2
    (vec-set! v2 2 0)
    v1
  ]
}

@defproc[(vec-data [v vec?]) array?]{

  Returns the underlying C representation of @var[v].

  Example:
  @example[
    (define v (vec 1 2 3))
    (sequence->list (in-array (vec-data v)))
  ]
}

@defproc[(vec-length [v vec?]) exact-nonnegative-integer?]{

  Returns the number of @components in @var[v].

  Example:
  @example[
    (vec-length (vec 0 1 2))
  ]
}

@defproc[(make-vec-data [x real?] ...) array?]{

  Allocates an array of @racket[_float]s that works with @racket[array-ref]
  and @racket[array-set!].

  Example:
  @example[
    (array-ref (make-vec-data 9 8 7) 2)
  ]
}

@defproc[(vec-copy [v vec?]) vec?]{

  Returns a fresh copy of @var[v].

  Example:
  @example[
    (define v1 (vec 1 2 3))
    (eq? (vec-data v1) (vec-data v1))
    (define v2 (vec-copy v1))
    (eq? (vec-data v1) (vec-data v2))
  ]
}

@defproc[(vec-name [v vec?]) symbol?]{

  Returns the length-suffixed name of @var[v].

  Example:
  @example[
    (vec-name (vec #:length 5))
  ]
}

@defproc[(vec-ref [v vec?] [i exact-nonnegative-integer?]) real?]{

  Returns the @var[i]th @component of @var[v].

  Example:
  @example[
    (vec-ref (vec 9 8 7 6) 2)
  ]
}

@defproc[(vec-set! [v vec?] [i exact-nonnegative-integer?] [x real?]) void?]{

  Changes the @var[i]th @component of @var[v] to @var[x].

  Example:
  @example[
    (define v (vec 1 2 3))
    (vec-set! v 1 0)
    v
  ]
}

@defproc[(vec->list [v vec?]) (listof real?)]{

  Returns the @component values of @var[v].

  Example:
  @example[
    (vec->list (vec 1 2 3))
  ]
}

@defproc[(vec->f32vector [v vec?]) f32vector?]{

  Returns the underlying C representation of @var[v] cast as an
  @racket[f32vector].

  Example:
  @example[
    (define v (vec 9 8 7))
    (define fv (vec->f32vector v))
    (f32vector-set! fv 1 0.0)
    v
  ]
}

@defproc[(in-vec [v vec?]
                 [start exact-nonnegative-integer? 0]
                 [stop (or/c exact-integer? #f) #f]
                 [step (and/c exact-integer? (not/c zero?)) 1]) sequence?]{

  Returns a sequence equivalent to @var[v] when no optional arguments are supplied.

  The optional arguments @var[start], @var[stop], and @var[step] are as in
  @racket[in-vector].

  Example:
  @example[
    (for ([x (in-vec (vec 1 2 3))])
      (println x))
  ]
}

@deftogether[(
@defform[(for/vec maybe-length (for-clause ...) body-or-break ... body)]
@defform[(for*/vec maybe-length (for-clause ...) body-or-break ... body)]
)]{

  Like @racket[for/list] and @racket[for*/list], but the results are
  accumulated into a @vector instead of a list.

  @example[
    (for/vec ([x (in-list '(9 8 7))])
      (sub1 x))
  ]

  @example[
    (for*/vec ([x (in-range 3)]
               [y (in-range 3)])
      (* x (+ x y)))
  ]

  If the optional @racket[#:length] clause is specified, it determines the
  length of the result @vector.

  @example[
    (for/vec #:length 5 ([x (in-list '(1 2 3))])
      (add1 x))
  ]

  If an optional @racket[#:fill] clause is specified and its value is not
  @racket['no-fill], it determines the value of any unspecified @components.

  @example[
    (for/vec #:length 4 #:fill -1
        ([x (in-naturals)] #:break (= x 2))
      x)
  ]

  @example[
    (for*/vec #:length 10 #:fill -1
        ([x (in-range 3)]
         [y (in-range 3)])
      (+ x (* x y)))
  ]
}

@defproc[(vec-constructor [v vec?]) (unconstrained-domain-> vec?)]{

  Returns a constructor function for @vectors with the same length as @var[v].

  Example:
  @example[
    ((vec-constructor (vec 1 2 3)) 4 5)
  ]
}

@defproc[(vec=! [v vec?] [u vec?]) void?]{

  Overwrites the @component values of @var[v] with the values of @var[u].

  Example:
  @example[
    (define v (vec 1 2 3))
    (vec=! v (vec 4 5 6))
    v
  ]

  @example[
    (vec=! v (vec 7 8))
    v
  ]

  @example[
    (vec=! v (vec 9 10 11 12))
    v
  ]
}

@deftogether[(
@defproc[(vec+ [a (or/c vec? real?)] ...) (or/c vec? real?)]
@defproc[(vec- [a (or/c vec? real?)] ...) (or/c vec? real?)]
@defproc[(vec* [a (or/c vec? real?)] ...) (or/c vec? real?)]
@defproc[(vec/ [a (or/c vec? real?)] ...) (or/c vec? real?)]
)]{

  Like @racket[+], @racket[-], @racket[*], and @racket[/], but generalized to
  consume @vectors or numbers.

  @example[
    (vec+ 1 2 3)
  ]

  @example[
    (vec- 3 (vec 4 5))
  ]

  @example[
    (vec* (vec 6 7) 8)
  ]

  @example[
    (vec/ (vec 9 0) (vec 3 11))
  ]
}

@deftogether[(
@defproc[(vec+=! [a vec?] [b (or/c vec? real?)]) void?]
@defproc[(vec-=! [a vec?] [b (or/c vec? real?)]) void?]
@defproc[(vec*=! [a vec?] [b (or/c vec? real?)]) void?]
@defproc[(vec/=! [a vec?] [b (or/c vec? real?)]) void?]
)]{

  Like @racket[vec+], @racket[vec-], @racket[vec*], and @racket[vec/], except
  the result is stored in @var[a].

  @example[
    (define v (vec 1 2))
    (vec+=! v (vec 3 4))
    v
  ]

  @example[
    (vec-=! v 5)
    v
  ]

  @example[
    (vec*=! v (vec 6 7 8))
    v
  ]

  @example[
    (vec/=! v (vec 9 10))
    v
  ]
}

@deftogether[(
@defproc[(++vec! [v vec?]) vec?]
@defproc[(--vec! [v vec?]) vec?]
)]{

  Increments or decrements the @components of @var[v] by 1 and then returns
  @var[v].

  Example:
  @example[
    (define v (vec 1 2))
    (++vec! v)
    v
  ]
}

@deftogether[(
@defproc[(vec++! [v vec?]) vec?]
@defproc[(vec--! [v vec?]) vec?]
)]{

  Increments or decrements the @components of @var[v] by 1 and then returns a
  fresh @vector with the original @components of @var[v].

  Example:
  @example[
    (define v (vec 1 2))
    (vec++! v)
    v
  ]
}

@defparam[current-vec-precision precision exact-nonnegative-integer? #:value 2]{
 
  A parameter that controls the maximum number of digits displayed after the
  decimal point of a @vector @component.

  Example:
  @example[
    (current-vec-precision 3)
    (vec 1/2 2/3 3/4)
  ]

  @example[
    (parameterize ([current-vec-precision 6])
      (println (vec 1/2 2/3 3/4)))
  ]

  @example[#:hidden (current-vec-precision 2)]
}

@defproc[(_vec [len exact-positive-integer?]) ctype? #:value (_array _float len)]{

  Creates a @vector type whose Racket representation is an array that works
  with @racket[array-ref] and @racket[array-set!].

}

@defform[(define-vec-type id #:length len)]{

  Binds variables related to @vectors of a specific length.

  A @racket[define-vec-type] form defines 2 names:

  @itemlist[

    @item{@var[id], a @vector constructor function that takes up to @var[len]
    arguments and returns a new @vector.}

    @item{@var[id]?, a predicate procedure that returns @racket[#t] for
    @vectors of length @var[len] and @racket[#f] for any other value.}

  ]

  Example:
  @example[
    (define-vec-type vec5 #:length 5)
    (vec5? (vec5))
    (vec5? (vec 1 2 3))
  ]

  @var[id] is also a @racket[match] pattern identifier similar to
  @racket[vec], except it only matches @vectors with exactly @var[len]
  @components.

  @example[
    (match (vec5 1 2 3 4 5)
      [(vec5 a b c d e) (reverse (list a b c d e))])
  ]

  @example[
    (match (vec5 1)
      [(vec5 _ _ #:rest xs) (apply + xs)])
  ]

  @example[
    (eval:error
     (match (vec 1 2 3)
       [(vec5 #:rest xs) (apply + xs)]))
  ]

  @example[
    (eval:error
     (match (vec 1 2 3 4 5 6 7)
       [(vec5 #:rest xs) (apply + xs)]))
  ]
}

@; .............................................................................

@subsection{@racket[vec] Types}

@deftogether[(
@defthing[#:kind "procedure" vec1? predicate/c]
@defthing[#:kind "procedure" vec2? predicate/c]
@defthing[#:kind "procedure" vec3? predicate/c]
@defthing[#:kind "procedure" vec4? predicate/c]
)]

@deftogether[(
@defproc[(vec1 [x real?]) vec1?]
@defproc[(vec2 [x real?] [y real?]) vec2?]
@defproc[(vec3 [x real?] [y real?] [z real?]) vec3?]
@defproc[(vec4 [x real?] [y real?] [z real?] [w real?]) vec4?]
)]

@; =============================================================================

@section[#:tag "vector:Double_Precision_Floats"]{Double Precision Floats}

@defmodule[glm/dvec]

Arrays of 64-bit floating point numbers.

@defproc[(dvec? [v any/c]) boolean?]

@defproc[
  (dvec [#:length len (or/c exact-positive-integer? #f) #f]
        [#:fill fill (or/c real? 'no-fill) 0]
        [x (or/c dvec? real?)] ...) dvec?
]

@defproc[
  (make-dvec [data array?]
             [len exact-positive-integer?]
             [fixed? boolean?]
             [fill (or/c real? 'no-fill)])
  dvec?
]

@defproc[(dvec-data [v dvec?]) array?]

@defproc[(dvec-length [v dvec?]) exact-nonnegative-integer?]

@defproc[(make-dvec-data [x real?] ...) array?]

@defproc[(dvec-copy [v dvec?]) dvec?]

@defproc[(dvec-name [v dvec?]) symbol?]

@defproc[(dvec-ref [v dvec?] [i exact-nonnegative-integer?]) flonum?]

@defproc[(dvec-set! [v dvec?] [i exact-nonnegative-integer?] [x number?]) void?]

@defproc[(dvec->list [v dvec?]) (listof flonum?)]

@defproc[(dvec->f64vector [v dvec?]) f64vector?]

@defproc[(in-dvec [v dvec?]
                  [start exact-nonnegative-integer? 0]
                  [stop (or/c exact-integer? #f) #f]
                  [step (and/c exact-integer? (not/c zero?)) 1]) sequence?]

@deftogether[(
@defform[(for/dvec maybe-length (for-clause ...) body-or-break ... body)]
@defform[(for*/dvec maybe-length (for-clause ...) body-or-break ... body)]
)]

@defproc[(dvec-constructor [v dvec?]) (unconstrained-domain-> dvec?)]

@defproc[(dvec=! [v dvec?] [u dvec?]) void?]

@deftogether[(
@defproc[(dvec+ [a (or/c dvec? real?)] ...) (or/c dvec? real?)]
@defproc[(dvec- [a (or/c dvec? real?)] ...) (or/c dvec? real?)]
@defproc[(dvec* [a (or/c dvec? real?)] ...) (or/c dvec? real?)]
@defproc[(dvec/ [a (or/c dvec? real?)] ...) (or/c dvec? real?)]
)]

@deftogether[(
@defproc[(dvec+=! [a dvec?] [b (or/c dvec? real?)]) void?]
@defproc[(dvec-=! [a dvec?] [b (or/c dvec? real?)]) void?]
@defproc[(dvec*=! [a dvec?] [b (or/c dvec? real?)]) void?]
@defproc[(dvec/=! [a dvec?] [b (or/c dvec? real?)]) void?]
)]

@deftogether[(
@defproc[(++dvec! [v dvec?]) dvec?]
@defproc[(--dvec! [v dvec?]) dvec?]
)]

@deftogether[(
@defproc[(dvec++! [v dvec?]) dvec?]
@defproc[(dvec--! [v dvec?]) dvec?]
)]

@defparam[current-dvec-precision precision exact-nonnegative-integer? #:value 2]

@defproc[(_dvec [len exact-nonnegative-integer?]) ctype? #:value (_array _double len)]

@defform[(define-dvec-type id #:length len)]

@; .............................................................................

@subsection{@racket[dvec] Types}

@deftogether[(
@defthing[#:kind "procedure" dvec1? predicate/c]
@defthing[#:kind "procedure" dvec2? predicate/c]
@defthing[#:kind "procedure" dvec3? predicate/c]
@defthing[#:kind "procedure" dvec4? predicate/c]
)]

@deftogether[(
@defproc[(dvec1 [x real?]) dvec1?]
@defproc[(dvec2 [x real?] [y real?]) dvec2?]
@defproc[(dvec3 [x real?] [y real?] [z real?]) dvec3?]
@defproc[(dvec4 [x real?] [y real?] [z real?] [w real?]) dvec4?]
)]

@; =============================================================================

@section{Signed Integers}

@defmodule[glm/ivec]

Arrays of signed integers.

@defproc[(ivec? [v any/c]) boolean?]

@defproc[
  (ivec [#:length len (or/c exact-integer? #f) #f]
        [#:fill fill (or/c integer? 'no-fill) 0]
        [x (or/c ivec? integer?)] ...) ivec?
]

@defproc[
  (make-ivec [data array?]
             [len exact-positive-integer?]
             [fixed? boolean?]
             [fill (or/c integer? 'no-fill)])
  ivec?
]

@defproc[(ivec-data [v ivec?]) array?]

@defproc[(ivec-length [v ivec?]) exact-nonnegative-integer?]

@defproc[(make-ivec-data [x integer?] ...) array?]

@defproc[(ivec-copy [v ivec?]) ivec?]

@defproc[(ivec-name [v ivec?]) symbol?]

@defproc[(ivec-ref [v ivec?] [i exact-nonnegative-integer?]) flonum?]

@defproc[(ivec-set! [v ivec?] [i exact-nonnegative-integer?] [x number?]) void?]

@defproc[(ivec->list [v ivec?]) (listof flonum?)]

@defproc[(ivec->s32vector [v ivec?]) s32vector?]

@defproc[(in-ivec [v ivec?]
                  [start exact-nonnegative-integer? 0]
                  [stop (or/c exact-integer? #f) #f]
                  [step (and/c exact-integer? (not/c zero?)) 1]) sequence?]

@deftogether[(
@defform[(for/ivec maybe-length (for-clause ...) body-or-break ... body)]
@defform[(for*/ivec maybe-length (for-clause ...) body-or-break ... body)]
)]

@defproc[(ivec-constructor [v ivec?]) (unconstrained-domain-> ivec?)]

@defproc[(ivec=! [v ivec?] [u ivec?]) void?]

@deftogether[(
@defproc[(ivec+ [a (or/c ivec? integer?)] ...) (or/c ivec? integer?)]
@defproc[(ivec- [a (or/c ivec? integer?)] ...) (or/c ivec? integer?)]
@defproc[(ivec* [a (or/c ivec? integer?)] ...) (or/c ivec? integer?)]
@defproc[(ivec/ [a (or/c ivec? integer?)] ...) (or/c ivec? integer?)]
)]

@deftogether[(
@defproc[(ivec+=! [a ivec?] [b (or/c ivec? integer?)]) void?]
@defproc[(ivec-=! [a ivec?] [b (or/c ivec? integer?)]) void?]
@defproc[(ivec*=! [a ivec?] [b (or/c ivec? integer?)]) void?]
@defproc[(ivec/=! [a ivec?] [b (or/c ivec? integer?)]) void?]
)]

@deftogether[(
@defproc[(++ivec! [v ivec?]) ivec?]
@defproc[(--ivec! [v ivec?]) ivec?]
)]

@deftogether[(
@defproc[(ivec++! [v ivec?]) ivec?]
@defproc[(ivec--! [v ivec?]) ivec?]
)]

@defproc[(_ivec [len exact-nonnegative-integer?]) ctype? #:value (_array _int len)]

@defform[(define-ivec-type id #:length len)]

@; .............................................................................

@subsection{@racket[ivec] Types}

@deftogether[(
@defthing[#:kind "procedure" ivec1? predicate/c]
@defthing[#:kind "procedure" ivec2? predicate/c]
@defthing[#:kind "procedure" ivec3? predicate/c]
@defthing[#:kind "procedure" ivec4? predicate/c]
)]

@deftogether[(
@defproc[(ivec1 [x integer?]) ivec1?]
@defproc[(ivec2 [x integer?] [y integer?]) ivec2?]
@defproc[(ivec3 [x integer?] [y integer?] [z integer?]) ivec3?]
@defproc[(ivec4 [x integer?] [y integer?] [z integer?] [w integer?]) ivec4?]
)]

@; =============================================================================

@section{Unsigned Integers}

@defmodule[glm/uvec]

Arrays of unsigned integers.

@defproc[(uvec? [v any/c]) boolean?]

@defproc[
  (uvec [#:length len (or/c exact-positive-integer? #f) #f]
        [#:fill fill (or/c nonnegative-integer? 'no-fill) 0]
        [x (or/c uvec? nonnegative-integer?)] ...) uvec?
]

@defproc[
  (make-uvec [data array?]
             [len exact-positive-integer?]
             [fixed? boolean?]
             [fill (or/c nonnegative-integer? 'no-fill)])
  uvec?
]

@defproc[(uvec-data [v uvec?]) array?]

@defproc[(uvec-length [v uvec?]) exact-nonnegative-integer?]

@defproc[(make-uvec-data [x nonnegative-integer?] ...) array?]

@defproc[(uvec-copy [v uvec?]) uvec?]

@defproc[(uvec-name [v uvec?]) symbol?]

@defproc[(uvec-ref [v uvec?] [i exact-nonnegative-integer?]) nonnegative-integer?]

@defproc[(uvec-set! [v uvec?] [i exact-nonnegative-integer?] [x nonnegative-integer?]) void?]

@defproc[(uvec->list [v uvec?]) (listof flonum?)]

@defproc[(uvec->u32vector [v uvec?]) u32vector?]

@defproc[(in-uvec [v uvec?]
                  [start exact-nonnegative-integer? 0]
                  [stop (or/c exact-integer? #f) #f]
                  [step (and/c exact-integer? (not/c zero?)) 1]) sequence?]

@deftogether[(
@defform[(for/uvec maybe-length (for-clause ...) body-or-break ... body)]
@defform[(for*/uvec maybe-length (for-clause ...) body-or-break ... body)]
)]

@defproc[(uvec-constructor [v uvec?]) (unconstrained-domain-> uvec?)]

@defproc[(uvec=! [v uvec?] [u uvec?]) void?]

@deftogether[(
@defproc[(uvec+ [a (or/c uvec? nonnegative-integer?)] ...) (or/c uvec? nonnegative-integer?)]
@defproc[(uvec- [a (or/c uvec? nonnegative-integer?)] ...) (or/c uvec? nonnegative-integer?)]
@defproc[(uvec* [a (or/c uvec? nonnegative-integer?)] ...) (or/c uvec? nonnegative-integer?)]
@defproc[(uvec/ [a (or/c uvec? nonnegative-integer?)] ...) (or/c uvec? nonnegative-integer?)]
)]

@deftogether[(
@defproc[(uvec+=! [a uvec?] [b (or/c uvec? nonnegative-integer?)]) void?]
@defproc[(uvec-=! [a uvec?] [b (or/c uvec? nonnegative-integer?)]) void?]
@defproc[(uvec*=! [a uvec?] [b (or/c uvec? nonnegative-integer?)]) void?]
@defproc[(uvec/=! [a uvec?] [b (or/c uvec? nonnegative-integer?)]) void?]
)]

@deftogether[(
@defproc[(++uvec! [v uvec?]) uvec?]
@defproc[(--uvec! [v uvec?]) uvec?]
)]

@deftogether[(
@defproc[(uvec++! [v uvec?]) uvec?]
@defproc[(uvec--! [v uvec?]) uvec?]
)]

@defproc[(_uvec [len exact-nonnegative-integer?]) ctype? #:value (_array _uint len)]

@defform[(define-uvec-type id #:length len)]

@; .............................................................................

@subsection{@racket[uvec] Types}

@deftogether[(
@defthing[#:kind "procedure" uvec1? predicate/c]
@defthing[#:kind "procedure" uvec2? predicate/c]
@defthing[#:kind "procedure" uvec3? predicate/c]
@defthing[#:kind "procedure" uvec4? predicate/c]
)]

@deftogether[(
@defproc[(uvec1 [x nonnegative-integer?]) uvec1?]
@defproc[(uvec2 [x nonnegative-integer?]
                [y nonnegative-integer?]) uvec2?]
@defproc[(uvec3 [x nonnegative-integer?]
                [y nonnegative-integer?]
                [z nonnegative-integer?]) uvec3?]
@defproc[(uvec4 [x nonnegative-integer?]
                [y nonnegative-integer?]
                [z nonnegative-integer?]
                [w nonnegative-integer?]) uvec4?]
)]

@; =============================================================================

@section{Booleans}

@defmodule[glm/bvec]

Arrays of boolean values.

@defproc[(bvec? [v any/c]) boolean?]

@defproc[
  (bvec [#:length len (or/c exact-integer? #f) #f]
        [#:fill fill (or/c boolean? 'no-fill) #f]
        [x (or/c bvec? boolean?)] ...) bvec?
]

@defproc[
  (make-bvec [data array?]
             [len exact-nonnegative-integer?]
             [fixed? boolean?]
             [fill (or/c boolean? 'no-fill)])
  bvec?
]

@defproc[(bvec-data [v bvec?]) array?]

@defproc[(bvec-length [v bvec?]) exact-nonnegative-integer?]

@defproc[(make-bvec-data [x boolean?] ...) array?]

@defproc[(bvec-copy [v bvec?]) bvec?]

@defproc[(bvec-name [v bvec?]) symbol?]

@defproc[(bvec-ref [v bvec?] [i exact-nonnegative-integer?]) boolean?]

@defproc[(bvec-set! [v bvec?] [i exact-nonnegative-integer?] [x boolean?]) void?]

@defproc[(bvec->list [v bvec?]) (listof boolean?)]

@defproc[(bvec->s32vector [v bvec?]) s32vector?]

@defproc[(in-bvec [v bvec?]
                  [start exact-nonnegative-integer? 0]
                  [stop (or/c exact-integer? #f) #f]
                  [step (and/c exact-integer? (not/c zero?)) 1]) sequence?]

@deftogether[(
@defform[(for/bvec maybe-length (for-clause ...) body-or-break ... body)]
@defform[(for*/bvec maybe-length (for-clause ...) body-or-break ... body)]
)]

@defproc[(bvec-constructor [v bvec?]) (unconstrained-domain-> bvec?)]

@defproc[(bvec=! [v bvec?] [u bvec?]) void?]

@defproc[(_bvec [len exact-nonnegative-integer?]) ctype? #:value (_array _int len)]

@defform[(define-bvec-type id #:length len)]

@defproc[(bvec-and [a bvec?] ...+) bvec?]{

  Returns the pointwise logical conjunction of the @var[a]s.

  Example:
  @example[
    (bvec-and (bvec3 #t #t #t)
              (bvec3 #t #t #f)
              (bvec3 #f #t #t))
  ]
}

@defproc[(bvec-or [a bvec?] ...+) bvec?]{

  Returns the pointwise logical disjunction of the @var[a]s.

  Example:
  @example[
    (bvec-or (bvec3 #f #f #f)
             (bvec3 #t #f #f)
             (bvec3 #f #f #t))
  ]
}

@defproc[(bvec-not [a bvec?]) bvec?]{

  Returns the pointwise logical negation of @var[a].

  Example:
  @example[
    (bvec-not (bvec3 #f #t #f))
  ]
}

@; .............................................................................

@subsection{@racket[bvec] Types}

@deftogether[(
@defthing[#:kind "procedure" bvec1? predicate/c]
@defthing[#:kind "procedure" bvec2? predicate/c]
@defthing[#:kind "procedure" bvec3? predicate/c]
@defthing[#:kind "procedure" bvec4? predicate/c]
)]

@deftogether[(
@defproc[(bvec1 [x boolean?]) bvec1?]
@defproc[(bvec2 [x boolean?] [y boolean?]) bvec2?]
@defproc[(bvec3 [x boolean?] [y boolean?] [z boolean?]) bvec3?]
@defproc[(bvec4 [x boolean?] [y boolean?] [z boolean?] [w boolean?]) bvec4?]
)]
