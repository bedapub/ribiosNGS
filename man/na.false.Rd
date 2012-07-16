\name{na.false}
\alias{na.false}
\title{
  Replace NA with FALSE
}
\description{
  Replace \code{NA} in a vector with \code{FALSE}
}
\usage{
na.false(x)
}
\arguments{
  \item{x}{A logical vector or matrix}
}
\value{
  Logical vector or matrix with NAs replaced by FALSE
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  myX <- c("HSV", "FCK", "FCN", NA, "BVB")
  res <- myX == "HSV"
  na.false(res)
}
