#lang at-exp racket/base

@require[
  "./glm-includes.rkt"
  scribble/manual
  template
  @for-syntax[racket/base]
  @for-label[glm racket/contract template]]

@provide[document-vector-type-constructors]

@void[@example[#:hidden @require[glm]]]

@for/template[([N '(1 2 3 4)])
  @define[N-components @list["N " @if-template[(= N 1) "component" "components"]]]
  @define[N-components. @list[@N-components "."]]
]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@define-template[@document-vector-type-constructors[Type $ N]]{

  @defproc[($vecN? [v any/c]) boolean?]{

    Returns @racket[#t] if @var[v] is a Type vector with @N-components,
    @racket[#f] otherwise.

    Examples:
    @example[
      ($vecN? ($vecN))
      ($vecN? 10)
    ]
  }

  @defthing[#:kind "procedure" $vecN
    (cond-template
      [(= N 1) (case-> (-> $vecN?)
                       (-> any/c $vecN?))]
      [(= N 2) (case-> (-> $vecN?)
                       (-> any/c $vecN?)
                       (-> any/c any/c $vecN?))]
      [(= N 3) (case-> (-> $vecN?)
                       (-> any/c $vecN?)
                       (-> any/c any/c $vecN?)
                       (-> any/c any/c any/c $vecN?))]
      [(= N 4) (case-> (-> $vecN?)
                       (-> any/c $vecN?)
                       (-> any/c any/c $vecN?)
                       (-> any/c any/c any/c $vecN?)
                       (-> any/c any/c any/c any/c $vecN?))])
  ]{

    Returns a Type vector with @N-components.

  }
}
