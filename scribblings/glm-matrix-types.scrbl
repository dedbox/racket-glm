#lang scribble/manual

@title{Matrix Types}

@require{./glm-includes.rkt}

@require[
  @for-syntax[
    racket/base
    syntax/transformer
  ]
  @for-label[
    (except-in ffi/unsafe ->)
    glm
    racket/base
    racket/contract/base
    racket/match
    racket/sequence
  ]
]

@; -----------------------------------------------------------------------------
@; Local Shortcuts

@define[_i @var[i]]
@define[ith @list[@_i "th"]]

@define[_m @var[m]]
@define[_m. @list[@_m "."]]

@define[_v @var[v]]
@define[_v. @list[@_v "."]]

@define[_x @var[x]]
@define[_x. @list[@_x "."]]

@define[xs @list[@var[x] "s"]]
@define[length-of-the-xs @racket[(length '(#,(var x) ...))]]
@define[L @racketid[L]]
@define[_M @var[M]]
@define[_N @var[N]]

@define[1×M @list["1×" @_M]]
@define[L×M @list[@L "×" @_M]]
@define[M×M @list[@_M "×" @_M]]
@define[N×M @list[@_N "×" @_M]]

@; #############################################################################

@defmodule[glm/matrix-types]

This module re-exports @racketmodname[glm/dmat] and @racketmodname[glm/mat].

@; =============================================================================

@section[#:tag "matrix:Single_Precision_Floats"]{Single Precision Floats}

@defmodule[glm/mat]

A @deftech{matrix} is a two-dimensional array of 32-bit floating point
@component values.

Two @matrices are @racket[equal?] iff they have the same dimensions, the same
number of components, and each pair of consecutive @components are @racket[=].

@defproc[(mat? [m any/c]) boolean?]{

  Returns @racket[#t] if @_m is a @matrix.

}

@defproc[
  (mat [#:cols N (or/c exact-positive-integer? #f) #f]
       [#:rows M exact-positive-integer?]
       [#:fill fill (or/c real? #f) 0]
       [x (or/c mat? vec? real?)] ...) mat?
]{

  Allocates a @matrix with the @xs as its @components in column-major order.

  @example[
    (mat #:rows 2 1 2 3 4 5 6)
    (mat #:rows 3 1 2 3 4 5 6)
  ]

  When no @xs are given, the result is an @N×M (cols×rows) @matrix with 1's
  along its diagonal and 0's everywhere else.

  @example[
    (mat #:cols 3 #:rows 2)
    (mat #:cols 2 #:rows 3)
  ]

  When @_N is given and the @xs consist of a lone scalar value @_x, the result
  is an @N×M @matrix with @_x along its diagonal and @var[fill] everywhere
  else.

  @example[
    (mat #:cols 3 #:rows 2 -1/2)
  ]

  If the lone @_x is a @matrix, the result is an @N×M @matrix with @_x
  embedded in its upper-left corner.

  @example[
    (mat #:cols 3 #:rows 4 #:fill -3 (mat #:rows 2 2))
    (mat #:cols 3 #:rows 2 #:fill -3 (mat #:rows 4 2))
  ]

  When @_N = @length-of-the-xs, each of the @xs becomes the sole argument to a
  column @vector constructor.

  @example[
    (mat #:cols 3 #:rows 2 9 8 7)
    (mat #:cols 2 #:rows 3 (vec2 1 2) (vec3 3 4 5))
  ]

  When @_N is @racket[#f] and @length-of-the-xs ≤ 1, the result is an @M×M
  matrix.

  @example[
    (mat #:rows 2)
    (mat #:rows 3 2)
  ]

  If 2 ≤ @length-of-the-xs ≤ @_M, the result is an @1×M matrix.

  @example[
    (mat #:rows 3 1 2)
  ]

  If @L = @length-of-the-xs > @_M, the result is an @L'×@_M matrix, where @L'
  = @L mod @_M + (0 if @_M divides @L else 1).

  @example[
    (mat #:rows 2 1 2 3 4)
    (mat #:rows 3 1 2 3 4)
  ]

  @racket[mat] is also a @racket[match] pattern identifier for deconstructing
  @matrices.

  @example[
    (match (mat #:rows 3 1 2 3 4 5 6)
      [(mat x y z r g b) (list (list r g b) (list x y z))])
  ]

  Optional @racket[#:cols] or @racket[#:rows] patterns may be given, and
  @matrices of indeterminate length can be matched with a final
  @racket[#:rest] pattern.

  @example[
    (match (mat #:rows 3 1 2 3 4 5 6)
      [(mat #:cols cols #:rows rows x y z #:rest rgb)
       (list cols rows rgb x y z)])
  ]

  @example[
    (match (mat #:rows 3 1 2 3 4 5 6)
      [(mat #:cols cols #:rows rows #:rest _) (* cols rows)])
  ]
}

@defproc[(make-mat [data array?]
                   [num-cols exact-positive-integer?]
                   [num-rows exact-positive-integer?]) mat?]{

  Constructs a fresh @matrix on the first @var[num-cols]×@var[num-rows]
  @components of some existing @var[data]. The array is not copied; the new
  Racket representation is backed by the existing C representation.

  Example:
  @example[
    (define m1 (mat #:rows 5))
    (define m2 (make-mat (mat-data m1) 3 3))
    m2
    (mat-set! m1 1 1 -1)
    m2
  ]
}

@defproc[(mat-data [m mat?]) array?]{

  Returns the underlying C representation of @_m.

  Example:
  @example[
    (define m (mat #:rows 2 1 2 3 4 5 6))
    (sequence->list (in-array (mat-data m)))
  ]
}

@deftogether[(
@defproc[(mat-length   [m mat?]) exact-positive-integer?]
@defproc[(mat-num-cols [m mat?]) exact-positive-integer?]
@defproc[(mat-num-rows [m mat?]) exact-positive-integer?]
)]{

  Returns the number of columns or rows in @_m.

  Example:
  @example[
    (define m (mat #:rows 2 1 2 3 4 5 6))
    m
    (mat-length m)
    (mat-num-cols m)
    (mat-num-rows m)
  ]
}

@defproc[(make-mat-data [num-cols exact-positive-integer?]
                        [num-rows exact-positive-integer?]
                        [x real?] ...+) array?]{

  Allocates a 2-D array of @racket[_float]s that works with @racket[array-ref]
  and @racket[array-set!].

  Example:
  @example[
    (array-ref (make-mat-data 3 2 1 2 3 4 5 6) 2 1)
  ]
}

@defproc[(mat-copy [m mat?]) mat?]{

  Returns a fresh copy of @_m.

  Example:
  @example[
    (define m1 (mat #:rows 3 1 2 3))
    (eq? (mat-data m1) (mat-data m1))
    (define m2 (mat-copy m1))
    (eq? (mat-data m1) (mat-data m2))
  ]
}

@defproc[(mat-name [m mat?]) symbol?]{

  Returns the dimension-suffixed name of @_m.

  Example:
  @example[
    (mat-name (mat #:rows 5))
    (mat-name (mat #:cols 5 #:rows 4))
    (mat-name (mat #:cols 4 #:rows 5))
  ]
}

@defproc*[(
[(mat-ref [m mat?]
          [col exact-nonnegative-integer?]
          [row exact-nonnegative-integer?]) vec?]
[(mat-ref [m mat?] [i exact-nonnegative-integer?]) vec?]
)]{

  Returns a @component value of @_m.

  Example:
  @example[
    (define m (mat #:rows 2 1 2 3 4 5 6))
    m
    (mat-ref m 5)
    (mat-ref m 2 1)
  ]
}

@defproc[(mat-row [m mat?] [i exact-nonnegative-integer?]) vec?]{

  Returns a copy of the @ith row @vector of @_m.

  Example:
  @example[
    (define m (mat #:rows 2 1 2 3 4 5 6))
    m
    (mat-row m 1)
  ]
}

@defproc[(mat-column [m mat?] [i exact-nonnegative-integer?]) vec?]{

  Returns the @ith column @vector of @_m. The data is not copied; the new
  Racket representation is backed by the existing C representation.

  Example:
  @example[
    (define m (mat #:rows 2 1 2 3 4 5 6))
    m
    (mat-column m 1)
    (vec-set! (mat-column m 1) 0 -1)
    m
  ]
}

@defproc*[(
[(mat-set! [m mat?]
           [col exact-nonnegative-integer?]
           [row exact-nonnegative-integer?]
           [x real?]) void?]
[(mat-set! [m mat?] [i exact-nonnegative-integer?] [x real?]) void?]
)]{

  Changes a @component of @_m to @_x.

  Example:
  @example[
    (define m (mat #:rows 2 1 2 3 4 5 6))
    (mat-set! m 5 -1)
    (mat-set! m 2 0 -2)
    m
  ]
}

@defproc[(mat-set-row! [m mat?] [i exact-nonnegative-integer?] [v vec?]) void?]{

  Changes the @ith row of @_m to the @components of @_v.

  Example:
  @example[
    (define m (mat #:rows 2 1 2 3 4 5 6))
    (mat-set-row! m 1 (vec -2 -4 -6))
    m
  ]
}

@defproc[(mat-set-column! [m mat?] [i exact-nonnegative-integer?] [v vec?]) void?]{

  Changes the @ith column of @_m to the @components of @_v.

  Example:
  @example[
    (define m (mat #:rows 2 1 2 3 4 5 6))
    (mat-set-column! m 1 (vec -3 -4))
    m
  ]
}

@deftogether[(
@defproc[(mat-columns [m mat?]) (listof vec?)]
@defproc[(mat-rows [m mat?]) (listof vec?)]
@defproc[(mat->list [m mat?]) (listof real?)]
)]{

  Returns the @component columns, rows, or values of @_m.

  Example:
  @example[
    (define m (mat #:rows 3 1 2 3 4 5 6))
    m
    (mat-columns m) 
    (mat-rows m)
    (mat->list m)
  ]

  @racket[mat-columns] is also a @racket[match] pattern identifier for
  deconstructing the columns of a @matrix.

  @example[
    (match m
      [(mat-columns xyz rgb) (list rgb xyz)])
  ]

  @Matrices of indeterminate length can be matched with a final
  @racket[#:rest] pattern.

  @example[
    (match m
      [(mat-columns (vec x y z) #:rest vs) (list vs z y x)])
  ]
}

@defproc[(mat->f32vector [m mat?]) f32vector?]{

  Returns the underlying C representation of @_m cast as an
  @racket[f32vector].

  Example:
  @example[
    (define m (mat #:rows 2 1 2 3 4 5 6))
    (define fv (mat->f32vector m))
    (f32vector-set! fv 5 0.0)
    m
  ]
}

@defproc[(in-mat [m mat?]) sequence?]{

  Returns a sequence equivalent to @_m.

  Example:
  @example[
    (for ([x (in-mat (mat #:rows 2))])
      (println x))
  ]
}

@defproc[(in-mat-rows [m mat?]) sequence?]{

  Returns a sequence equivalent to the rows of @_m.

  Example:
  @example[
    (for ([v (in-mat-rows (mat #:rows 2))])
      (println v))
  ]
}

@defproc[(in-mat-columns [m mat?]) sequence?]{

  Returns a sequence equivalent to the columns of @_m.

  Example:
  @example[
    (for ([v (in-mat-columns (mat #:rows 2))])
      (println v))
  ]
}

@deftogether[(
@defform[
  (for/mat maybe-cols #:rows length-expr maybe-fill
      (for-clause ...)
    body-or-break ... body)
  #:grammar ([maybe-cols (code:line)
                         (code:line #:cols length-expr)]
             [maybe-fill (code:line)
                         (code:line #:fill length-expr)])
]
@defform[
  (for*/mat maybe-cols #:rows length-expr maybe-fill
      (for-clause ...)
    body-or-break ... body)
]
)]{

  Like @racket[for/list] and @racket[for*/list], but the results are
  accumulated into a @matrix instead of a list.

  @example[
    (for/mat #:rows 2
        ([i (in-range 6)])
      (add1 i))
    (for*/mat #:rows 3
        ([i (in-range 2)]
         [j (in-range 3)])
      (+ j (* i 100)))
  ]

  If the optional @racket[#:cols] clause is specified, it determines the shape
  of the result @matrix.

  @example[
    (for/mat #:cols 5 #:rows 3
        ([x (in-range 5)])
      (* x x))
  ]

  If the optional @racket[#:fill] clause is specified, it determines the value
  of any unspecified @components.

  @example[
    (for/mat #:rows 3 #:fill -1
        ([x (in-range 4)])
      x)
  ]
}

@deftogether[(
@defproc[(mat-column-predicate [m mat?]) predicate/c]
@defproc[(mat-row-predicate [m mat?]) predicate/c]
@defproc[(mat-predicate [m mat?]) predicate/c]
)]{

  Returns a predicate function that returns @racket[#t] when applied to a
  @matrix or @vector with the same dimension(s) as @_m.

  Example:
  @example[
    (define m (mat #:cols 5 #:rows 4))
    (define mat4x5? (mat-predicate m))
    (define vec5? (mat-row-predicate m))
    (mat4x5? m)
    (vec5? (mat-row m 0))
    (vec5? (mat-column m 0))
  ]
}

@deftogether[(
@defproc[(mat-column-constructor [m mat?]) (unconstrained-domain-> vec?)]
@defproc[(mat-row-constructor [m mat?]) (unconstrained-domain-> vec?)]
@defproc[(mat-constructor [m mat?]) (unconstrained-domain-> mat?)]
)]{

  Returns a constructor function for @matrices or @vectors with the same
  dimension(s) as @_m.

  Example:
  @example[
    (define m (mat #:cols 5 #:rows 4))
    (define mat4x5 (mat-constructor m))
    (define vec5 (mat-row-constructor m))
    (mat4x5)
    (vec5)
  ]
}

@defproc[(mat=! [m mat?] [n mat?]) void?]{

  Overwrites the @component values of @_m with the values of @var[n].

  Example:
  @example[
    (define m (mat #:rows 2 1 2 3 4 5 6))
    m
    (mat=! m (mat #:rows 2 9 8 7 6 5 4))
    m
  ]
}

@deftogether[(
@defproc[(mat+ [a (or/c mat? vec? real?)] ...+) (or/c mat? vec? real?)]
@defproc[(mat- [a (or/c mat? vec? real?)] ...+) (or/c mat? vec? real?)]
@defproc[(mat* [a (or/c mat? vec? real?)] ...+) (or/c mat? vec? real?)]
@defproc[(mat/ [a (or/c mat? vec? real?)] ...+) (or/c mat? vec? real?)]
)]{

  Like @racket[+], @racket[-], @racket[*], and @racket[/], but generalized to
  consume @matrices, @vectors, or numbers.

  @example[
    (mat+ 1 2 3)
  ]

  @example[
    (mat- 3 (vec 4 5))
  ]

  @example[
    (mat* (mat #:rows 3 2) (vec 1 2 3))
  ]

  @example[
    (mat/ (mat #:rows 3) 2)
    (mat/ (mat #:rows 3) (mat #:rows 3 2))
  ]
}

@deftogether[(
@defproc[(mat+=! [m mat?] [a (or/c mat? vec? real?)]) void?]
@defproc[(mat-=! [m mat?] [a (or/c mat? vec? real?)]) void?]
@defproc[(mat*=! [m mat?] [a (or/c mat? vec? real?)]) void?]
@defproc[(mat/=! [m mat?] [a (or/c mat? vec? real?)]) void?]
)]{

  Like @racket[mat+], @racket[mat-], @racket[mat*], and @racket[mat/], except
  the result is stored in @racket[m].

  @example[
    (define m (mat #:cols 3 #:rows 2 1 2 3))
    m
    (mat+=! m (mat #:cols 3 #:rows 2 10 10 10))
    m
  ]

  @example[
    (mat-=! m (mat #:cols 3 #:rows 2 1 2 3))
    m
  ]

  @example[
    (define m (mat #:rows 3 1 2 3 4 5 6 7 8 9))
    m
    (mat*=! m (mat #:rows 3 2))
    m
  ]

  @example[
    (define m (mat #:rows 3 1 2 3 4 5 6 7 8 9))
    m
    (mat/=! m (mat #:rows 3 2))
    m
  ]
}

@deftogether[(
@defproc[(++mat! [m mat?]) mat?]
@defproc[(--mat! [m mat?]) mat?]
)]{

  Increments or decrements the @components of @_m by 1 and then returns @_m.

  Example:
  @example[
    (define m (mat #:rows 2 1 2 3 4))
    (++mat! m)
    m
  ]
}

@deftogether[(
@defproc[(mat++! [m mat?]) mat?]
@defproc[(mat--! [m mat?]) mat?]
)]{

  Increments or decrements the @components of @_m by 1 and then returns a
  fresh @matrix with the original @components of @_m.

  Example:
  @example[
    (define m (mat #:rows 2 1 2 3 4))
    (mat++! m)
    m
  ]
}

@defproc[(_mat [cols exact-positive-integer?]
               [rows exact-positive-integer?]) ctype?]{

  Creates a @matrix type whose Racket representation is an array that works
  with @racket[array-ref] and @racket[array-set!].

}

@defform[(define-mat-type id #:cols N #:rows M)]{

  Binds variables related to @matrices of specific dimensions.

  A @racket[define-mat-type] form defines 2 names:

  @itemlist[

    @item{@var[id], a @matrix constructor function that takes up to @N×M
    arguments and returns a new @matrix.}

    @item{@var[id]?, a predicate procedure that returns @racket[#t] for
    @matrices with @_N columns and @_M rows, and @racket[#f] for any other
    value.}

  ]

  Example:
  @example[
    (define-mat-type mat5 #:cols 5 #:rows 5)
    (mat5? (mat5))
    (mat5? (mat #:rows 3))
  ]

  @var[id] is also a @racket[match] patterns similar to @racket[mat], except
  it only matches @matrices with exactly @_N columns and @_M rows.

  @example[
    (match (mat5 1 2 3 4 5)
      [(mat5 a b c #:rest xs) (list a b c (apply + xs))])
  ]

  @example[
    (eval:error
     (match (mat #:rows 3)
       [(mat5 #:rest xs) (apply + xs)]))
  ]
}

@; .............................................................................

@subsection{@racket[mat] Types}

@deftogether[(
@defthing[#:kind "procedure" mat2? predicate/c]
@defthing[#:kind "procedure" mat3? predicate/c]
@defthing[#:kind "procedure" mat4? predicate/c]
@defthing[#:kind "procedure" mat2x2? predicate/c]
@defthing[#:kind "procedure" mat2x3? predicate/c]
@defthing[#:kind "procedure" mat2x4? predicate/c]
@defthing[#:kind "procedure" mat3x2? predicate/c]
@defthing[#:kind "procedure" mat3x3? predicate/c]
@defthing[#:kind "procedure" mat3x4? predicate/c]
@defthing[#:kind "procedure" mat4x2? predicate/c]
@defthing[#:kind "procedure" mat4x3? predicate/c]
@defthing[#:kind "procedure" mat4x4? predicate/c]
)]

@deftogether[(
@defproc[(mat2 [a (or/c mat? vec? real?)] ...) mat2?]
@defproc[(mat3 [a (or/c mat? vec? real?)] ...) mat3?]
@defproc[(mat4 [a (or/c mat? vec? real?)] ...) mat4?]
@defproc[(mat2x2 [a (or/c mat? vec? real?)] ...) mat2x2?]
@defproc[(mat2x3 [a (or/c mat? vec? real?)] ...) mat2x3?]
@defproc[(mat2x4 [a (or/c mat? vec? real?)] ...) mat2x4?]
@defproc[(mat3x2 [a (or/c mat? vec? real?)] ...) mat3x2?]
@defproc[(mat3x3 [a (or/c mat? vec? real?)] ...) mat3x3?]
@defproc[(mat3x4 [a (or/c mat? vec? real?)] ...) mat3x4?]
@defproc[(mat4x2 [a (or/c mat? vec? real?)] ...) mat4x2?]
@defproc[(mat4x3 [a (or/c mat? vec? real?)] ...) mat4x3?]
@defproc[(mat4x4 [a (or/c mat? vec? real?)] ...) mat4x4?]
)]

@deftogether[(
@defthing[_mat2 ctype?]
@defthing[_mat3 ctype?]
@defthing[_mat4 ctype?]
@defthing[_mat2x2 ctype?]
@defthing[_mat2x3 ctype?]
@defthing[_mat2x4 ctype?]
@defthing[_mat3x2 ctype?]
@defthing[_mat3x3 ctype?]
@defthing[_mat3x4 ctype?]
@defthing[_mat4x2 ctype?]
@defthing[_mat4x3 ctype?]
@defthing[_mat4x4 ctype?]
)]

@; =============================================================================

@section[#:tag "matrix:Double_Precision_Floats"]{Double Precision Floats}

@defmodule[glm/dmat]

Two-dimensional arrays of 64-bit floating point numbers.

@defproc[(dmat? [m any/c]) boolean?]

@defproc[
  (dmat [#:cols N (or/c exact-positive-integer? #f) #f]
        [#:rows M exact-positive-integer?]
        [#:fill fill (or/c real? #f) 0]
        [x (or/c dmat? dvec? real?)] ...) dmat?
]

@defproc[(make-dmat [data array?]
                    [num-cols exact-positive-integer?]
                    [num-rows exact-positive-integer?]) dmat?]

@defproc[(dmat-data [m dmat?]) array?]

@deftogether[(
@defproc[(dmat-length   [m dmat?]) exact-positive-integer?]
@defproc[(dmat-num-rows [m dmat?]) exact-positive-integer?]
@defproc[(dmat-num-cols [m dmat?]) exact-positive-integer?]
)]

@defproc[(make-dmat-data [num-cols exact-positive-integer?]
                         [num-rows exact-positive-integer?]
                         [x real?] ...+) array?]

@defproc[(dmat-copy [m dmat?]) dmat?]

@defproc[(dmat-name [m dmat?]) symbol?]

@defproc*[(
[(dmat-ref [m dmat?]
           [col exact-nonnegative-integer?]
           [row exact-nonnegative-integer?]) dvec?]
[(dmat-ref [m dmat?] [i exact-nonnegative-integer?]) dvec?]
)]

@defproc[(dmat-row [m dmat?] [i exact-nonnegative-integer?]) dvec?]

@defproc[(dmat-column [m dmat?] [i exact-nonnegative-integer?]) dvec?]

@defproc[(dmat-set-column! [m dmat?] [i exact-nonnegative-integer?] [v dvec?]) void?]

@defproc[(dmat-set-row! [m dmat?] [i exact-nonnegative-integer?] [v dvec?]) void?]

@defproc*[(
[(dmat-set! [m dmat?]
            [col exact-nonnegative-integer?]
            [row exact-nonnegative-integer?]
            [x real?]) void?]
[(dmat-set! [m dmat?] [i exact-nonnegative-integer?] [x real?]) void?]
)]

@deftogether[(
@defproc[(dmat->list [m dmat?]) (listof real?)]
@defproc[(dmat-rows [m dmat?]) (listof dvec?)]
@defproc[(dmat-columns [m dmat?]) (listof dvec?)]
)]

@defproc[(in-dmat-columns [m dmat?]) sequence?]

@defproc[(in-dmat-rows [m dmat?]) sequence?]

@defproc[(in-dmat [m dmat?]) sequence?]

@deftogether[(
@defform[
  (for/dmat maybe-cols #:rows length-expr maybe-fill
      (for-clause ...)
    body-or-break ... body)
  #:grammar ([maybe-cols (code:line)
                         (code:line #:cols length-expr)]
             [maybe-fill (code:line)
                         (code:line #:fill length-expr)])
]
@defform[
  (for*/dmat maybe-cols #:rows length-expr maybe-fill
      (for-clause ...)
    body-or-break ... body)
]
)]

@deftogether[(
@defproc[(dmat-column-predicate [m dmat?]) predicate/c]
@defproc[(dmat-row-predicate [m dmat?]) predicate/c]
@defproc[(dmat-predicate [m dmat?]) predicate/c]
)]

@deftogether[(
@defproc[(dmat-column-constructor [m dmat?]) (unconstrained-domain-> dvec?)]
@defproc[(dmat-row-constructor [m dmat?]) (unconstrained-domain-> dvec?)]
@defproc[(dmat-constructor [m dmat?]) (unconstrained-domain-> dmat?)]
)]

@defproc[(dmat=! [m dmat?] [n dmat?]) void?]

@deftogether[(
@defproc[(dmat+ [a (or/c dmat? dvec? real?)] ...+) (or/c dmat? dvec? real?)]
@defproc[(dmat- [a (or/c dmat? dvec? real?)] ...+) (or/c dmat? dvec? real?)]
@defproc[(dmat* [a (or/c dmat? dvec? real?)] ...+) (or/c dmat? dvec? real?)]
)]

@deftogether[(
@defproc[(dmat+=! [m dmat?] [a (or/c dmat? dvec? real?)]) void?]
@defproc[(dmat-=! [m dmat?] [a (or/c dmat? dvec? real?)]) void?]
@defproc[(dmat*=! [m dmat?] [a (or/c dmat? dvec? real?)]) void?]
)]

@deftogether[(
@defproc[(++dmat! [m dmat?]) dmat?]
@defproc[(--dmat! [m dmat?]) dmat?]
)]

@deftogether[(
@defproc[(dmat++! [m dmat?]) dmat?]
@defproc[(dmat--! [m dmat?]) dmat?]
)]

@defproc[(_dmat [rows exact-positive-integer?]
                [cols exact-positive-integer?]) ctype?]

@defform[(define-dmat-type id #:cols N #:rows M)]

@; .............................................................................

@subsection{@racket[dmat] Types}

@deftogether[(
@defthing[#:kind "procedure" dmat2? predicate/c]
@defthing[#:kind "procedure" dmat3? predicate/c]
@defthing[#:kind "procedure" dmat4? predicate/c]
@defthing[#:kind "procedure" dmat2x2? predicate/c]
@defthing[#:kind "procedure" dmat2x3? predicate/c]
@defthing[#:kind "procedure" dmat2x4? predicate/c]
@defthing[#:kind "procedure" dmat3x2? predicate/c]
@defthing[#:kind "procedure" dmat3x3? predicate/c]
@defthing[#:kind "procedure" dmat3x4? predicate/c]
@defthing[#:kind "procedure" dmat4x2? predicate/c]
@defthing[#:kind "procedure" dmat4x3? predicate/c]
@defthing[#:kind "procedure" dmat4x4? predicate/c]
)]

@deftogether[(
@defproc[(dmat2 [a (or/c dmat? dvec? real?)] ...) dmat2?]
@defproc[(dmat3 [a (or/c dmat? dvec? real?)] ...) dmat3?]
@defproc[(dmat4 [a (or/c dmat? dvec? real?)] ...) dmat4?]
@defproc[(dmat2x2 [a (or/c dmat? dvec? real?)] ...) dmat2x2?]
@defproc[(dmat2x3 [a (or/c dmat? dvec? real?)] ...) dmat2x3?]
@defproc[(dmat2x4 [a (or/c dmat? dvec? real?)] ...) dmat2x4?]
@defproc[(dmat3x2 [a (or/c dmat? dvec? real?)] ...) dmat3x2?]
@defproc[(dmat3x3 [a (or/c dmat? dvec? real?)] ...) dmat3x3?]
@defproc[(dmat3x4 [a (or/c dmat? dvec? real?)] ...) dmat3x4?]
@defproc[(dmat4x2 [a (or/c dmat? dvec? real?)] ...) dmat4x2?]
@defproc[(dmat4x3 [a (or/c dmat? dvec? real?)] ...) dmat4x3?]
@defproc[(dmat4x4 [a (or/c dmat? dvec? real?)] ...) dmat4x4?]
)]

@deftogether[(
@defthing[_dmat2 ctype?]
@defthing[_dmat3 ctype?]
@defthing[_dmat4 ctype?]
@defthing[_dmat2x2 ctype?]
@defthing[_dmat2x3 ctype?]
@defthing[_dmat2x4 ctype?]
@defthing[_dmat3x2 ctype?]
@defthing[_dmat3x3 ctype?]
@defthing[_dmat3x4 ctype?]
@defthing[_dmat4x2 ctype?]
@defthing[_dmat4x3 ctype?]
@defthing[_dmat4x4 ctype?]
)]
