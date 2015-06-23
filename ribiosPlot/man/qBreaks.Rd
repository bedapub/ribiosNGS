\name{qBreaks}
\alias{qBreaks}
\title{
  Internal function to re-calculate breaks of histograms when x-axis is clipped
}
\description{
  Internal function to re-calculate breaks of histograms when x-axis is clipped
}
\usage{
qBreaks(x, quantiles = c(0, 0.99), breaks = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Value to draw histgrams with}
  \item{quantiles}{Quantiles of x that determine the clip boundary of x-axis}
  \item{breaks}{Integer, number of breaks applied to original data}
}
\details{
  The function calculates the new break numbers caused by the clipping
  of x axis. This is usally larger than the original number of breaks .
}
\value{
  Integer: number of breaks
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  This function is directly used by \code{qHist}
}
\examples{
\dontrun{
testVal <- rnorm(1000)
qBreaks(testVal, quantiles=c(0.25, 0.75), breaks=100) ## should be about
400
}
}