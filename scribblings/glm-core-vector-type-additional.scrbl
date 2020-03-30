#lang at-exp racket/base

@require[
  "./glm-includes.rkt"
  scribble/manual
  template
  @for-syntax[racket/base]
  @for-label[glm racket/base racket/contract template]]

@provide[document-vector-type-additional]

@define-template[@make-or/c[N]
  @cond-template[
    [(= N 1) 0]
    [(= N 2) (or/c 0 1)]
    [(= N 3) (or/c 0 1 2)]
    [(= N 4) (or/c 0 1 2 3)]]]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@define-template[@document-vector-type-additional[Type $ N]]{
  @defproc[($vecN-ref [v $vecN?] [i (make-or/c N)]) $scalar?]{

    Returns the @var[i]th component of @var[v].

  }

  @defproc[($vecN-set! [v $vecN?] [i (make-or/c N)] [x $scalar?]) $scalar?]{

    Changes the component position @var[i] in @var[v] to @var[x]. The first
    component in the vector corresponds to @racketid[x], so the position
    @var[i] must be less than N, otherwise @racket[exn:fail:contract] is
    raised.

  }

  @defproc[($vecN->list [v $vecN?]) (listof $scalar?)]{

    Returns the components of @var[v] as a list.

  }

  @defproc[($vecN-map [f procedure?] [v $vecN?] (... ...)) $vecN?]{

    Applies @var[f] to the components of the @var[v]s. @var[f] must accept the
    same number of arguments as the number of supplied @var[v]s. The result is
    a @racket[$vecN] containing the results of @var[f].

  }

  @defproc[(in-$vecN [v $vecN?]) sequence?]{

    Returns a sequence equivalent to the components of @var[v].

  }

  @defform[(for/$vecN for-clauses body (... ...))]{

    Iterates like @racket[for/list], but the results are accumulated into a
    @racket[$vecN] instead of a list.

  }

  @defform[(for*/$vecN for-clauses body (... ...))]{

    Like @racket[for/$vecN], but with the implicit nesting of @racket[for*].

  }
}
