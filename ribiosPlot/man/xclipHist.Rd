\name{xclipHist}
\alias{xclipHist}
\title{
  Histogram with clipped x axis
}
\description{
  Draw histograms with clipped x axis; the clipping is determined by
  quantiles of x values.
}
\usage{
xclipHist(x, xclip = c(0.01, 0.99), breaks = 100, quantiles = 0.25, qlty = 2, qlwd = 2, qcol = "red", ...)
}
\arguments{
  \item{x}{Value to draw the histogram}
  \item{xclip}{Quantiles of x-values that should be displayed; values
    outside of this range are not shown in the histogram}
  \item{breaks}{A integer number indicating how many breaks should the
    \emph{original unclipped} data have; the function will automatically
    re-calculate the breaks of the clipped data so that they look consistent.}
  \item{quantiles}{Numeric values or \code{NULL}; in case of numeric
    values, at the corresponding quantile values vertical lines and text
    labels are drawn; if set to \code{NULL}, no extra items will
    display. See examples below.}
  \item{qlty}{Type of vertical quantile lines}
  \item{qlwd}{Width of vertical quantile lines}
  \item{qcol}{Color of vertical quantile lines}
  \item{\dots}{Other parameters that are passed to \code{hist}}
}
\details{
  The function clips (subsets) x-axis and recalcualte the breaks so that
  the clipped image looks like a real subset of the original data.
}
\value{
  The object returned by the \code{hist} function, with an extra item
  named \code{quantiles}.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{qHist}, which draws quantile line and texts onto histograms.
}
\examples{
testVal <- c(rnorm(1000),10)
hist(testVal, breaks=100)
xclipRes <- xclipHist(testVal, xclip=c(0.001, 0.999), quantiles=0.50)

xclipRes$quantiles
}

