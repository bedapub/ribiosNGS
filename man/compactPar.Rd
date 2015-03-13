\name{compactPar}
\alias{compactPar}
\title{
  Compact par setting
}
\description{
  Compact par setting
}
\usage{
compactPar(mar=c(3,3,1.5,1.5), mgp=c(2,1,0), ...)
}
\arguments{
  \item{mar}{marginal option passed to \code{par}}
  \item{mgp}{margin line option passed to \code{par}}
  \item{...}{other parameters passed to \code{par}} 
}
\details{
  For compact figures
}
\author{
  Jitao David Zhang
}
\seealso{
  \code{\link{par}}
}
\examples{
  compactPar()
  plot(1:4)
}