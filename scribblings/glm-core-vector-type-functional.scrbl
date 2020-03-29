#lang at-exp racket/base

@require[
  "./glm-includes.rkt"
  scribble/manual
  template
  @for-syntax[racket/base]
  @for-label[glm racket/contract template]]

@provide[document-vector-type-functional]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@define-template[@document-vector-type-functional[Type $ N]]{
  @for/template[([□ '(+ - * / % & // ^ << >>)]
                 [Op '(sum difference product quotient remainder |bitwise and|
                           |bitwise or| |bitwise xor| |left shift| |right shift|)])]{
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

      Returns the component-wise Op of @var[v1] and either @var[v2],
      @racket[($vecN v2)], or @racket[($vecN x)].

    }
  }

  @defproc[($vecN~ [v $vecN?]) $vecN?]{

    Returns the component-wise bitwise negation of @var[v].

  }

  @defproc[($vecN++ [v $vecN?]) $vecN?]{

    Returns a copy of @var[v] with its components incremented.

  }

  @defproc[($vecN-- [v $vecN?]) $vecN?]{

    Returns a copy of @var[v] with its components decremented.

  }

  @when-template[(eq? '$ 'b)]{
    @for/template[([□ '(and or)]
                   [Op '(|logical conjunction| |logical disjunction|)])]{
      @defproc[(bvecN-□ [v1 bvecN?] [v2 bvecN?]) bvecN?]{

        Returns the component-wise Op of @var[v1] and @var[v2].

      }
    }
  }
}
