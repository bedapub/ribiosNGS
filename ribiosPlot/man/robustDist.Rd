\name{robustDist}
\alias{robustDist}
\title{
  Robust distance
}
\description{
  A wrapper function for the \code{dist} function in the \code{stats}
  package. It replaces NA values in the distance matrix by the maximum
  distance in the same matrix, therefore prevents cases where hclust
  fails because of \code{NA} distances.
}
\usage{
robustDist(x, ...)
}
\arguments{
  \item{x}{a numeric matrix, data frame or \sQuote{dist} object}
  \item{\dots}{Other parameters passed to the \code{dist} function. }
}
\details{
  In the rare case of all-NA distance matrices, all values are assigned
  arbitrarily to one.
}
\value{
  The same as \code{\link{dist}}, however without \code{NA}s.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
mymat <- matrix(c(3,2,1,NA,NA,NA,
                4,1,2,NA,NA,NA,
                NA,NA,NA,5,1,2), ncol=6, byrow=TRUE)
dist(mymat)
robustDist(mymat)
}
