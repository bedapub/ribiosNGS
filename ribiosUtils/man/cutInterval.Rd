\name{cutInterval}
\alias{cutInterval}
\title{Cut numbers into interval factors}
\description{
  Cut numbers into interval factors
}
\usage{
cutInterval(x, step = 1, labelOption = c("cut.default", "left", "right"),
include.lowest = FALSE, right = TRUE, dig.lab = 3, ordered_result = FALSE, ...)
}
\arguments{
  \item{x}{A vector of numbers}
  \item{step}{Step size.}
  \item{labelOption}{How is the label displayed.See \code{details} section.}
  \item{include.lowest}{Logical, passed to \code{cut}}
  \item{right}{Logial, passed to \code{cut}}
  \item{dig.lab}{See \code{\link{cut}}}
  \item{ordered_result}{See \code{\link{cut}}}
  \item{\dots}{Other parameters that are passed to \code{\link{cut}}}
}
\details{
  Cut a vector of numbers into interval factors.

  Three types of labels (levels) are supported: \dQuote{cut.default} 
  (Interval labels returned by \code{cut} as default), \dQuote{left} 
  (Left boundary of intervals), and \dQuote{right} 
  (Right boundary of intervals).
}
\value{
  A vector of factors
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{cut}}
}
\examples{
testNum <- rnorm(100)
(testFac <- cutInterval(testNum, step=1, labelOption="cut.default"))

## compare the result to
(testFacCut <- cut(testNum, 10))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
