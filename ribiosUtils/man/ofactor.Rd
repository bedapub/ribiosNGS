\name{ofactor}
\alias{ofactor}
\title{
  Ordered factor
}
\description{
  Build a factor using the order of input character strings
}
\usage{
ofactor(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A vector of character strings}
  \item{\dots}{Other parameters passed to \code{factor}}
}
\value{
  Factor with levels in the same order of the input strings.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{factor}
}
\examples{
testStrings <- c("A", "C", "B", "B", "C")
(testFac <- factor(testStrings))
(testOfac <- ofactor(testStrings))

stopifnot(identical(levels(testOfac), c("A", "C", "B")))
}

