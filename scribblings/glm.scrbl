#lang scribble/manual

@title[#:style 'toc]{OpenGL Mathematics (GLM)}
@author{@author+email["Eric Griffis" "dedbox@gmail.com"]}

@require{./glm-includes.rkt}

@require[
  template
  @for-label[
    racket/base
    (except-in racket/contract any)
    glm]
  @for-syntax[racket/base]]

@example[#:hidden @require[glm]]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@defmodule[glm]

@table-of-contents[]

@include-section{./glm-core.scrbl}
@include-section{./glm-stable.scrbl}
@include-section{./glm-recommended.scrbl}
@include-section{./glm-experimental.scrbl}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@require[scribble/example]

@close-eval[glm-evaluator]
