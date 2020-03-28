#lang at-exp racket/base

@require[
  "./glm-includes.rkt"
  scribble/manual
  template
  @for-syntax[racket/base]
  @for-label[glm racket/contract template]]

@provide[document-vector-type-updaters]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@define-template[@document-vector-type-updaters[Type $ N]]{
  @deftogether[(
    @defproc[($vecN=! [v1 $vecN?] [v2 tvecN?]) $vecN?]
    @defproc[($vecN=tvecN! [v1 $vecN?] [v2 tvecN?]) $vecN?]
    @defproc[($vecN=$vecN! [v1 $vecN?] [v2 tvecN?]) $vecN?]
  )]{

    Updates each component of @var[v1] to match the corresponding component of
    @var[v2], then returns @var[v1].

  }

  @for/template[([□ '(+ - * /)]
                 [Op '(sum difference product quotient)])]{
    @deftogether[(
      @defproc[($vecN□=! [v $vecN?] [a @if-template[(= N 1)
                                       (or/c tvec1? $scalar?)
                                       (or/c tvec1? tvecN? $scalar?)]]) $vecN?]
      @defproc[($vecN□=tvecN! [v1 $vecN?] [v2 tvecN?]) $vecN?]
      @defproc[($vecN□=$vecN! [v1 $vecN?] [v2 $vecN?]) $vecN?]
      @unless-template[(= N 1)
        @defproc[($vecN□=tvec1! [v1 $vecN?] [v2 tvec1?]) $vecN?]]
      @defproc[($vecN□=$scalar! [v $vecN?] [x $scalar?]) $vecN?]
    )]{

      Updates each component of @var[v1] to the Op of itself and the
      corresponding component of @var[v2], then returns @var[v1].

    }
  }

  @defproc[($vecN++! [v $vecN?]) $vecN?]{

    Increments each component of @var[v], then returns @var[v].

  }

  @defproc[($vecN--! [v $vecN?]) $vecN?]{

    Decrements each component of @var[v], then returns @var[v].

  }
}
