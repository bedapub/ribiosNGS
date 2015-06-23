\name{qHist}
\alias{qHist}
\title{
  Histogram with quantile line(s) and text(s)
}
\description{
  A handy function to plot histogram with display elements of quantile.
}
\usage{
qHist(x, quantiles = 0.25, breaks = 100, qlty = 2, qlwd = 2, qcol = "red", ...)
}
\arguments{
  \item{x}{Value to draw the histogram}
  \item{quantiles}{Numeric values or \code{NULL}; in case of numeric
    values, at the corresponding quantile values vertical lines and text
    labels are drawn; if set to \code{NULL}, no extra items will
    display. See examples below.}
  \item{breaks}{Integer, number of breaks}
  \item{qlty}{Type of vertical quantile lines}
  \item{qlwd}{Width of vertical quantile lines}
  \item{qcol}{Color of vertical quantile lines}
  \item{\dots}{Other parameters that are passed to \code{hist}}
}
\details{
  The appends vertical lines and texts to histograms produced by
  \code{hist}. This can be useful in unspecific filtering of expression data.
}
\value{
  The object returned by the \code{hist} function, with an extra item
  named \code{quantiles}.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{hist}
}
\examples{
testVal <- rnorm(1000)
hist(testVal)
qHist(testVal, quantiles=c(0.25, 0.75), border="lightgray")
}