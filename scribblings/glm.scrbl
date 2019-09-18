#lang scribble/manual

@title{The OpenGL Math Library}
@author{@author+email["Eric Griffis" "dedbox@gmail.com"]}

@; -----------------------------------------------------------------------------
@; Other Docs

@define[C-types
  @secref["types" #:doc '(lib "scribblings/foreign/foreign.scrbl")]]

@; -----------------------------------------------------------------------------
@; External Limks

@define[glm.g-truc.net
  @hyperlink[
    "https://glm.g-truc.net"
    ]{glm.g-truc.net}]

@define[version-4.2
  @hyperlink[
    "http://www.opengl.org/registry/doc/GLSLangSpec.4.20.8.clean.pdf"
    ]{version 4.2}]

@; #############################################################################

OpenGL Mathematics (GLM)

@glm.g-truc.net

@defmodule[glm]

@section{Core features}

Features that implement in Racket the GLSL specification as closely as
possible.

The GLM core consists of @C-types that mirror GLSL types and Racket functions
that mirror the GLSL functions.

The best documentation for the GLM Core is the current GLSL specification,
@list[version-4.2].

To use the GLM core, @racket[require] @racketmodname[glm].

@subsection{Vector types}

Vector types of two to four components with an exhaustive set of operators.

@subsubsection{Vector types with precision qualifiers}

Vector types with precision qualifiers which may result in various precision
in terms of ULPs.

GLSL allows defining qualifiers for particular variables. With OpenGL's GLSL,
these qualifiers have no effect; they are for compatibility. With OpenGL ES's
GLSL, these qualifiers do have an effect.

@subsection{Matrix types}

Matrix types with C columns and R rows where C and R are values between 2 to 4
included. These types have exhauseive sets of operators.

@subsubsection{Matrix types with precision qualifiers}

Matrix types with precision qualifiers which may result in various precision
in term of ULPs.

GLSL allows defining qualifiers for particular variables. With OpenGL's GLSL,
these qualifiers have no effect; they are for compatibility. With OpenGL ES's
GLSL, these qualifiers do have an effect.

@section{Stable extensions}

Additional features not specified by GLSL specification.

EXT extensions are fully tested and documented.

Even if it's highly unrecommended, it's possible to include all the extensions
at once by importing @racketmodname[glm/ext]. Otherwise, each extension needs
to be imported from a specific module.

@section{Recommended extensions}

Additional features not specified by GLSL specification.

GTC extensions aim to be stable with tests and documentation.

Even if it's highly unrecommended, it's possible to include all the extensions
at once by importing @racketmodname[glm/ext]. Otherwise, each extension needs
to be imported from a specific module.
