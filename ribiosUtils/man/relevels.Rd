\name{relevels}
\alias{relevels}
\title{Adjust factor levels}
\description{
  Re-order factor levels so that the levels specified by \code{ref} are
  first and the others are moved down.
}
\usage{
relevels(x, refs)
}
\arguments{
  \item{x}{A vector of factors}
  \item{refs}{Levels that should be put first.}
}
\details{
  The function expands the functionality of \code{relevel} by accepting
  multiple levels at one time. See examples.
}
\value{
  A vector of factors, with levels adjusted.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com> 
}
\seealso{
  \code{\link{relevel}}
}
\examples{
testFac <- factor(c("HSV", "FCB", "BVB", "S04", "HSV"))
newTestFac <- relevels(testFac, c("HSV", "S04"))

print(testFac)
print(newTestFac)
}
