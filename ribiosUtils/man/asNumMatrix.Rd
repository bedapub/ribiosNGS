\name{asNumMatrix}
\alias{asNumMatrix}
\alias{atofMatrix}
\alias{stringDataFrame2numericMatrix}
\title{Convert data frame or matrix into a numeric matrix}
\description{
  Convert string-valued data frame or matrix into a numeric matrix
}
\usage{
asNumMatrix(x)
}
\arguments{
  \item{x}{A data.frame or matrix, most likely with string values}
}
\value{
  A numeric matrix with the same dimension
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
testDf <- data.frame(a=c("2.34", "4.55"), b=c("7.33", "9.10"))
asNumMatrix(testDf)

testMatrix <- matrix(c("2.34", "4.55", "9E-3","-2.44", "7.33", "9.10"), nrow=2)
asNumMatrix(testMatrix)
}

