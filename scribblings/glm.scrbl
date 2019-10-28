#lang scribble/manual

@title{OpenGL Mathematics (GLM) for Racket}
@author{@author+email["Eric Griffis" "dedbox@gmail.com"]}

@require{./glm-includes.rkt}

@require[scribble/example]

@; -----------------------------------------------------------------------------
@; External Limks

@deflink[GLM]{https://glm.g-truc.net}
@deflink[OpenGL-Mathematics-<GLM>]{https://glm.g-truc.net}
@deflink[OpenGL-Shading-Language-<GLSL>]{https://www.khronos.org/registry/OpenGL/index_gl.php}

@deflink[swizzling]{https://www.khronos.org/opengl/wiki/Data_Type_(GLSL)#Swizzling}
@define[swizzling. @list[swizzling "."]]

@; #############################################################################

@emph{OpenGL Mathematics (GLM) for Racket} is a Racket port of
@OpenGL-Mathematics-<GLM>, a mathematics library for graphics software based
on the @OpenGL-Shading-Language-<GLSL> specifications.

GLM for Racket provides @GLM's core functions and data types along with
support for popular Racket idioms such as sequence-based looping, variadic
keyword-based constructors, and @racket[match]-based de-structuring.

@table-of-contents[]

@; =============================================================================

@section{Swizzling}

@defmodulelang[glm/swizzle]

The @racketmodname[glm/swizzle] meta-language customizes the syntax of
identifiers to enable component @swizzling.

@codeblock{
  #lang glm/swizzle racket/base

  (define v (vec4 1 2 3 4))

  v.x ;; expands to (vec-ref v 0)
  v.xy ;; expands to (vec2 (vec-ref v 0) (vec-ref v 1))
}

All of the bindings exported by @racketmodname[glm/vector-types] are available
whenever @swizzling is enabled.

@; =============================================================================

@include-section{./glm-core.scrbl}

@close-eval[glm-evaluator]
