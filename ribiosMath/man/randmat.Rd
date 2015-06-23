\name{randmat}
\alias{randmat}
\title{Making a random matrix by sampling}
\description{
  Generating a column-wise random matrix by sampling a numeric vector.
}
\usage{
randmat(vec, size, N)
}
\arguments{
  \item{vec}{A numeric vector to be sampled from}
  \item{size}{Integer, size of sampling. See details.}
  \item{N}{Integer, number of sampling repeats. See details.}
}
\details{
  The function generates a matrix of the dimension
  \code{size}x\code{N}. Each column of the matrix is a random sampling
  of the input vector.

  Currently only sampling \emph{with replacement} is supported. 
}
\value{
  A random matrix of the dimension \code{size}x\code{n}.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
testVec <- 1:10
randmat(testVec, size=5L, N=5)
}