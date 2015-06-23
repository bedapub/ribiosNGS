\name{sortByDimnames}
\alias{sortByDimnames}
\title{
  Sort matrix by dim names
}
\description{
  Rearrange rows and columns of a matrix by dim names
}
\usage{
sortByDimnames(x, row.decreasing = FALSE, col.decreasing = FALSE)
}
\arguments{
  \item{x}{A matrix or data.frame}
  \item{row.decreasing}{Logical, whether rows should be sorted decreasingly}
  \item{col.decreasing}{Logical, whether columns should be sorted decreasingly}
}
\value{
  Resorted matrix or data frame
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
testMat <- matrix(1:16, nrow=4, dimnames=list(c("B", "D", "A", "C"), c("t", "f", "a", "g")))
sortByDimnames(testMat)
sortByDimnames(testMat, row.decreasing=TRUE, col.decreasing=FALSE)
}