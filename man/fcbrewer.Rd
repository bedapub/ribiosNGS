\name{fcbrewer}
\alias{fcbrewer}
\title{
  Factor color brewer
}
\description{
  The function generates a vector of color names for a factor(-like)
  object.
}
\usage{
fcbrewer(factor, panel = "Set1")
}
\arguments{
  \item{factor}{A vector of factors. Non-factors will be cast to factors
  by calling the \code{factor} function.}
  \item{panel}{This parameter can take three types of values: (1) a
    color set name defined in \code{brewer.pal.info} in the RColorBrewer
    package, (2) a function (or the name of a function) that takes an
    integer as input and returns a vector of colors that will be used as
    the base colors of levels of the factor, or (3) a character vector
    which represents the base colors. In the last case, the length of
    the vector must match the number of levels of the factor. See
    examples below.}
}
\details{
  When using \code{brewer.pal} to generate palettes, the panel is
  automatically expanded using
  \code{\link[grDevices]{colorRampPalette}} when the number of
  levels of the input factor exceeds the limit of respective panel. This
  is done automatically. 
}
\value{
  An \code{fcol} object encoding colors matching the factors as well as
  the base colors. The latter is often needed in figure legends.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{brewer.pal.info} for color panels.
}
\examples{
testFactor <- gl(4,25)
testCol1 <- fcbrewer(testFactor, panel="Set2")
testCol2 <- fcbrewer(testFactor, panel=heat.colors)
testCol3 <- fcbrewer(testFactor, panel="heat.colors")
testCol4 <- fcbrewer(testFactor, panel=c("black", "green", "orange", "lightblue"))

testRan <- runif(100)
## use colors of each item and colors of each level
plot(testRan, pch=21, bg=testCol1)
legend("topright", legend=paste("Class", 1:4),
       pch=21, pt.bg=fcbase(testCol1))

## boxplot uses colors matching to each level only
boxplot(testRan ~ testFactor, col=fcbase(testCol1))
}
