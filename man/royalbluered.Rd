\name{royalbluered}
\alias{royalbluered}
\alias{royalredblue}
\alias{turyeb}
\alias{redgreen}
\alias{greenred}
\alias{bluered}
\alias{redblue}
\alias{redblackblue}
\alias{blueblackred}
\alias{whiteblueblackheat}
\alias{threecolor.panels}
\alias{display.threecolor.panels}
\title{Three-color panels}
\description{
  Precompiled three-color panels for visualization purposes
}
\usage{
royalbluered(n)
royalredblue(n)
turyeb(n)
redgreen(n)
greenred(n)
bluered(n)
redblue(n)
redblackblue(n)
blueblackred(n)
whiteblueblackheat(n)

threecolor.panels()
display.threecolor.panels(nc=20)
}
\arguments{
  \item{n}{Number of colors needed}
  \item{nc}{Number of color columns}
}
\details{
  \code{threecolor.panels} returns all available three-color
  panels. \code{display.threecolor.panels} display available three-color
  panels.

  
  For the rest see function definitions
}
\value{Character vector of length \code{n} coding colors}
\author{Jitao David Zhang}
\seealso{
  \code{\link{blackyellow}} for two-color systems
}
\examples{
display.threecolor.panels()
}

