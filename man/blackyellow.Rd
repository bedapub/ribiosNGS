\name{blackyellow}
\alias{blackyellow}
\alias{yellowblack}
\alias{whiteblue}
\alias{whitered}
\alias{blackred}
\alias{blackgreen}
\alias{whiteblack}
\alias{blackwhite}
\alias{twocolor.panels}
\alias{display.twocolor.panels}
\title{Two-color panels}
\description{
  Precompiled two-color panels for visualization purposes
}
\usage{
blackyellow(n)
yellowblack(n)
whiteblue(n)
whitered(n)
blackred(n)
blackgreen(n)
whiteblack(n)
blackwhite(n)

twocolor.panels()
display.twocolor.panels(nc=20)
}
\arguments{
  \item{n}{Number of colors needed}
  \item{nc}{Number of color columns}
}
\details{
  \code{twocolor.panels} returns all available two-color
  panels. \code{display.twocolor.panels} display available two-color
  panels.
  
  For the rest see function definitions.
}
\value{Character vector of length \code{n} coding colors}
\author{Jitao David Zhang}
\seealso{
  \code{\link{royalbluered}} for three-color systems
}
\examples{
display.twocolor.panels()
}

