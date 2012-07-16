\name{headhead}
\alias{headhead}
\alias{tailtail}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  head/tail function for matrix or data.frame
}
\description{
  These two functions reassembles \code{head} and \code{tail}, showing
  the first rows and columns of 2D data structures, e.g. matrix or data.frame.
}
\usage{
headhead(x, m = 6L, n = 6L)
tailtail(x, m = 6L, n = 6L)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A \code{data.frame} or \code{matrix}}
  \item{m}{Integer, number of rows to show}
  \item{n}{Integer, number of columns to show}
}
\details{
  While \code{head} and \code{tail} can be applied to \code{data.frame}
  or \code{matrix} as well, they show all columns of the first (last)
  rows even if the matrix has a large number of columns. These two
  function, \code{headhead} and \code{tailtail}, circumvent this problem
  by showing only the first rows AND the first columns.
}
\value{
  The first rows/columns of the input object
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{head}},  \code{\link{tail}}
}
\examples{
myMat <- matrix(rnorm(10000), nrow=10L)
\dontrun{
  head(myMat)
}
headhead(myMat)
tailtail(myMat)
}
