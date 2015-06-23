\name{midentical}
\alias{midentical}
\title{
  Multiple identical
}
\description{
  Testing whether multiple objects are identical
}
\usage{
midentical(..., num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{Objects to be tested, or a list of them}
  \item{num.eq, single.NA, attrib.as.set, ignore.bytecode,
    ignore.environment}{See \code{\link{identical}}}
}
\details{
  \code{midentical} extends \code{identical} to test multiple objects
  instead of only two.
}
\value{
  A logical value, \code{TRUE} if all objects are identical
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{identical}
}
\examples{
set1 <- "HSV"
set2 <- set3 <- set4 <- c("HSV", "FCB")

midentical(set1, set2)
midentical(list(set1, set2))

midentical(set2, set3, set4)
midentical(list(set2, set3, set4))

## other options passed to identical
midentical(0, -0, +0, num.eq=FALSE)
midentical(0, -0, +0, num.eq=TRUE)
}
