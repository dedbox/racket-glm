#lang scribble/manual

@title{GLM Core}

@require{./glm-includes.rkt}

@; #############################################################################

@defmodule[glm]

This module re-exports @racketmodname[glm/matrix-types] and
@racketmodname[glm/vector-types].

@include-section{./glm-vector-types.scrbl}
@include-section{./glm-matrix-types.scrbl}
