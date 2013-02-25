\name{cossim}
\alias{cossim}
\alias{cosdist}
\title{Cosine similarity and distance}
\description{
  Cosine similarity (distance) between two vectors
}
\usage{
cossim(x, y, na.rm = TRUE)
cosdist(x, y, na.rm = TRUE)
}
\arguments{
  \item{x}{Vector or matrix of integer or numeric objects}
  \item{y}{Vector or matrix of integer or numeric objects}
  \item{na.rm}{Logical, whether NA should be removed.}
}
\details{
  If given as vectors, \code{x} and \code{y} must be of the same
  length. If given as matrices, both must have the same number of
  rows. If given as a pair of matrix and vector, the length of the
  vector must match the row number of the matrix.Otherwise the function
  aborts and prints error message.

  If \code{na.rm} is set \code{FALSE}, any \code{NA} in the input vectors
  will cause the result to be \code{NA}, or \code{NaN} if all values
  turn out to be \code{NA}.

  If \code{na.rm} is set \code{TRUE}, \code{NA} values are ignored.
}
\value{
  Numeric, the cosine similarity (distance) between two input vectors.
}
\references{
  \url{http://en.wikipedia.org/wiki/Cosine_similarity}
}
\note{
  Currently (February 2013) \code{na.rm=TRUE} is only valid when both inputs are
  vectors.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{cor}}
}
\examples{
testVal1 <- rnorm(10)
testVal2 <- rnorm(10)
testVal3 <- c(rnorm(9), NA)

cossim(testVal1, testVal2)
cossim(testVal1, testVal3, na.rm=TRUE)
cossim(testVal1, testVal3, na.rm=FALSE)

cosdist(testVal1, testVal2)
cosdist(testVal1, testVal3, na.rm=TRUE)
cosdist(testVal1, testVal3, na.rm=FALSE)

## test matrix
testMat1 <- matrix(rnorm(1000), nrow=10)
testMat2 <- matrix(rnorm(1000), nrow=10)
system.time(testMatCos <- cossim(testMat1, testMat2))

testMatVec <- cossim(testMat1, testMat2[,1L])
testVecMat <- cossim(testMat1[,1L], testMat2)

testVecMatDist <- cosdist(testMat1[,1L], testMat2)
}
