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
  \item{x}{Integer or numeric vector}
  \item{y}{Integer or numeric vector}
  \item{na.rm}{Logical, whether NA should be removed.}
}
\details{
  \code{x} and \code{y} must be of the same length, otherwise the
  function aborts.

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
}
