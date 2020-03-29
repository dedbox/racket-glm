#lang at-exp racket/base

@require[
  "./glm-includes.rkt"
  scribble/manual
  template
  @for-syntax[racket/base]
  @for-label[glm racket/contract template]]

@provide[document-vector-type-destructive]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@define-template[@document-vector-type-destructive[Type $ N]]{
  @deftogether[(
    @defproc[($vecN=! [v1 $vecN?] [v2 tvecN?]) $vecN?]
    @defproc[($vecN=tvecN! [v1 $vecN?] [v2 tvecN?]) $vecN?]
    @defproc[($vecN=$vecN! [v1 $vecN?] [v2 tvecN?]) $vecN?]
  )]{

    Updates each component of @var[v1] to match the corresponding component of
    @var[v2], then returns @var[v1].

  }

  @for/template[([□ '(+ - * / % & // ^ << >>)]
                 [Op '(sum difference product quotient remainder |bitwise AND|
                           |bitwise OR| |bitwise XOR| |left shift| |right shift|)])]{
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

      Updates @var[v1] with the component-wise Op of itself and either
      @var[v2], @racket[($vecN v2)], or @racket[($vecN x)], then returns
      @var[v1].

    }
  }

  @defproc[($vecN++! [v $vecN?]) $vecN?]{

    Increments the components of @var[v], then returns @var[v].

  }

  @defproc[($vecN--! [v $vecN?]) $vecN?]{

    Decrements the components of @var[v], then returns @var[v].

  }
}
