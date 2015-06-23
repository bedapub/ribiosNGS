\name{fcol-class}
\Rdversion{1.1}
\docType{class}
\alias{fcol-class}
\alias{fcol}
\alias{fcbase}
\alias{fcbase-methods}
\alias{fcbase,fcol-method}
\alias{show,fcol-method}

\title{Factor-matching colors}
\description{
  This object represents a vector of colors (in characters), together
  with a vector of 'base colors', i.e. the elementary colors used in the
  vector. This data structure is useful to represent factor using colors
  since the base colors are mapped to the levels of the factor.
}
\section{Objects from the Class}{
Objects can be created by calls of the function \code{fcol}. However,
the users should not directly call this function. Instead,
\code{\link{fcbrewer}} should be called to generate a factor-matching
color vector. \code{fcbrewer} calls \code{fcol} internally.
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Character vector representing colors}
    \item{\code{base}:}{Base colors}
  }
}
\section{Extends}{
Class \code{"\linkS4class{character}"}, from data part.
}
\section{Methods}{
  \describe{
    \item{fcbase}{\code{signature(object = "fcol")}: Return base colors}
    \item{show}{\code{signature(object = "fcol")}: Print the object}
	 }
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
  testFac <- gl(4,2)
  testCol <- fcbrewer(testFac, panel="Set1")

  print(testCol)
  fcbase(testCol)
  as.character(testCol)
}
\keyword{classes}
