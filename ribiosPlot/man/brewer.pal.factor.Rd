\name{brewer.pal.factor}
\alias{brewer.pal.factor}
\alias{brewer.pal.factorLevels}
\title{Build brewer.pal colors from factor}
\description{
  Build brewer.pal colors from factors (DEPRECATED)
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
  The functionality has been replaced by \code{fcbrewer}. The functions
  will be removed in the future release.
  
  The function is useful to build named RGB color values from
  factors. \code{brewer.pal.factor} return a color-HTML-string
  vector as the same length of the input factor vector, which is named
  by the input factor as well. \code{brewer.pal.factorLevels} returns a
  color vector of the length of the factor level, and the colors are
  named by the levels. See examples below.

  From version 1.1-16, the color palette is automatically
  reduced/expanded when the levels of input factors underlies or exceeds
  the minimum and maximum colors. See example below.
}
\value{
  Named HTML RGB colors.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
\dontrun{
myFac <- factor(c("HSV", "BVB", "FCB", "HSV", "BVB", "HSV"))
brewer.pal.factor(myFac, name="Set1")
brewer.pal.factorLevels(myFac, name="Set1")

myLongFac <- factor(paste("Sample", 1:20))
brewer.pal.factor(myLongFac, name="Set1")

myShortFac <- factor(paste("Sample", 1:2))
brewer.pal.factor(myShortFac, name="Set1")
}
}

