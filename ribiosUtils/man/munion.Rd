\name{munion}
\alias{mset}
\alias{munion}
\alias{mintersect}
\alias{msetdiff}
\title{
  Operations for multiple sets
}
\description{
  Set operation functions in the \code{base}
  package, \code{union}, \code{intersect} and \code{setdiff}, can only
  be applied to binary manipulations involving two sets. Following
  functions, \code{munion}, \code{mintersect} and \code{msetdiff},
  extend their basic versions to deal with multiple sets.
}
\usage{
munion(...)
mintersect(...)
msetdiff(...)
}
\arguments{
  \item{\dots}{Vectors of items, or a list of them. See examples below.}
}
\details{
  These functions apply set manipulations (union, intersect, or
  difference) in a sequential manner: the first two sets are considered
  first, then the third, the fourth and so on, till all sets have been
  visited.
}
\value{
  A vector of set operation results. Can be an empty vector if no
  results were returned.
}
\author{
    Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{union}}, \code{\link{intersect}} and \code{\link{setdiff}}.
}
\examples{
set1 <- c("HSV", "FCB", "BVB", "FCN", "HAN")
set2 <- c("HSV", "FCB", "BVB", "HAN")
set3 <- c("HSV", "BVB", "FSV")

munion(set1, set2, set3)
mintersect(set1, set2, set3)
msetdiff(set1, set2, set3)

## sets can be given in a list as well
munion(list(set1, set2, set3))
mintersect(list(set1, set2, set3))
msetdiff(list(set1, set2, set3))
}

