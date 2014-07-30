\name{intRange}
\alias{intRange}
\title{
  Returns a range defined by integers
}
\description{
  Return a range defined by integers
}
\usage{
intRange(x, na.rm = TRUE)
}
\arguments{
  \item{x}{A numeric vector}
  \item{na.rm}{Logical, whether NA should be removed}
}
\details{
  The function is similar to \code{\link{range}} but returns integer
  ranges that are just outside the real range: i.e. the floor of the
  left range and the ceiling of the right range.
}
\value{
  A vector of integers of length 2.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}

\examples{
intRange(rnorm(100))
}
