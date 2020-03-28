#lang at-exp racket/base

@require[
  "./glm-includes.rkt"
  scribble/manual
  template
  @for-syntax[racket/base]
  @for-label[glm racket/contract template]]

@provide[document-vector-type-arithmetic]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@define-template[@document-vector-type-arithmetic[Type $ N]]{
  @for/template[([□ '(+ - * /)]
                 [Op '(sum difference product quotient)])]{
    @deftogether[(
      @defproc[($vecN□ [v $vecN?] [a @if-template[(= N 1)
                                     (or/c tvec1? $scalar?)
                                     (or/c tvec1? tvecN? $scalar?)]]) $vecN?]
      @defproc[($vecN□tvecN [v1 $vecN?] [v2 tvecN?]) $vecN?]
      @defproc[($vecN□$vecN [v1 $vecN?] [v2 $vecN?]) $vecN?]
      @unless-template[(= N 1)
        @defproc[($vecN□tvec1 [v1 $vecN?] [v2 tvec1?]) $vecN?]]
      @defproc[($vecN□$scalar [v $vecN?] [x $scalar?]) $vecN?]
    )]{

      Returns the component-wise Op of @var[v1] and @var[v2].

    }
  }

  @defproc[($vecN++ [v $vecN?]) $vecN?]{

    Returns the component-wise increment of @var[v].

  }

  @defproc[($vecN-- [v $vecN?]) $vecN?]{

    Returns the component-wise decrement of @var[v].

  }
}
