\name{bound}
\alias{bound}
\alias{boundNorm}
\title{Bound values}
\description{
  Set boundaries for numeric values, or perform a 0-1 normalization.
}
\usage{
bound(x, low, high)
boundNorm(x, low=min(x,na.rm=TRUE), high=max(x, na.rm=TRUE))
}
\arguments{
  \item{x}{A numeric vector or matrix}
  \item{low}{New lower boundary}
  \item{high}{New higher boundary}
}
\details{
    The basic concepts of these functions are borrowed from the \code{bound} function in
    the \code{Qt} framework.

    \code{bound} sets the values smaller than \code{low}, or
    larger than \code{high}, to the value of \code{low} and \code{high}
    respectively.If no such values exist, the vector or matrix is
    returned unchanged.

    \code{boundNorm} performs a 0-1 normalization. Input vector or
    matrix is transformed linearly onto the region defined between
    \code{low} and \code{high}, which has the unit length (1).
    
}
\value{
  A numeric vector or matrix, the same type as input.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
myVec <- c(2,4,3,-1,9,5,3,4)
bound(myVec, 0, 8)
boundNorm(myVec)
## boundNorm returns negative values if input values lie out of the
## given region between low and high
boundNorm(myVec, 0, 8)

myMat <- matrix(myVec, nrow=2)
myMat
bound(myMat, 0, 8)
boundNorm(myMat)
boundNorm(myMat, 0, 8)
}
