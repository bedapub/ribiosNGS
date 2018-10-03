\name{histMat}
\alias{histMat}
\title{
  Make histograms for matrix
}
\description{
  Make histograms for matrix
}
\usage{
histMat(mat, 
  linesOpt = list(lwd = NULL, col = NULL, 
      lty = NULL, type = NULL, pch = NULL), 
  main = NULL, xlab = NULL, xlim = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{A numerical matrix}
  \item{linesOpt}{Line options}
  \item{main}{Title text}
  \item{xlab}{Xlab}
  \item{xlim}{Xlim}
  \item{\dots}{Other parameters passed to \code{hist}}
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{hist}}
}
\examples{
testMat <- matrix(rnorm(1000), nrow=100)
histMat(testMat)
}
