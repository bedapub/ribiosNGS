\name{brewer.pal.factor}
\alias{brewer.pal.factor}
\alias{brewer.pal.factorLevels}
\title{Build brewer.pal colors from factor}
\description{
  Build brewer.pal colors from factors.
}
\usage{
brewer.pal.factor(factor, name = "Greys")
brewer.pal.factorLevels(factor, name = "Greys")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{factor}{A factor vector}
  \item{name}{Color panel name to be passed to \code{brewer.pal}}
}
\details{
  The function is useful to build named RGB color values from
  factors. \code{brewer.pal.factor} return a color-HTML-string
  vector as the same length of the input factor vector, which is named
  by the input factor as well. \code{brewer.pal.factorLevels} returns a
  color vector of the length of the factor level, and the colors are
  named by the levels. See examples below.
}
\value{
  Named HTML RGB colors.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
myFac <- factor(c("HSV", "BVB", "FCB", "HSV", "BVB", "HSV"))
brewer.pal.factor(myFac, name="Set1")
brewer.pal.factorLevels(myFac, name="Set1")
}

