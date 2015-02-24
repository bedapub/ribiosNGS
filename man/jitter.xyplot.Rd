\name{jitter.xyplot}
\alias{jitter.xyplot}
\title{
  Make boxplots or dotplots with sample-size proportional jitters
}
\description{
  Make boxplots or dotplots with jitters of the size proportional to the
  sample size. See examples.
}
\usage{
jitter.xyplot(x, y, N = 20, factor = 1, ...)
}
\arguments{
  \item{x}{X-axis variable, numeric or factor}
  \item{y}{Y-axis variable, numeric}
  \item{N}{Number of groups that y-axis should be cut}
  \item{factor}{Jitter factor, passed to \link{jitter}}
  \item{\dots}{Other parameters passed to \code{panel.xyplot}}
}
\value{
  Side effects are used.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
library(lattice)

testX <- gl(8,5)
testY <- rnorm(40)
xyplot(testY ~ testX)
xyplot(testY ~ testX, panel=jitter.xyplot)

(xyBw <- bwplot(testY ~ testX))
(xyBwJitter <- update(xyBw, panel=jitter.xyplot))

testXnum <- rep(1:8, 5)
xyplot(testY ~ testXnum, panel=jitter.xyplot)
}

