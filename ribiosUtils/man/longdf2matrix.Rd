\name{longdf2matrix}
\alias{longdf2matrix}
\title{Convert a long-format data frame into matrix}
\description{
  Convert a long-format data frame into matrix
}
\usage{
longdf2matrix(df, row.col = 1L, column.col = 2L, value.col = 3L)
}
\arguments{
  \item{df}{Long-format data frame}
  \item{row.col}{Character or integer, which column of the input
    data.frame contains row names?}
  \item{column.col}{Character or integer, which column contains column names?}
  \item{value.col}{Character or integer, which column contains matrix values?}
}
\details{
  Input data.frame must contain at least three columns: one contains row
  names (specified by \code{row.col}), one contains column names
  (\code{column.col}), and one contains values in matrix cells
  (\code{value.col}). The output is a 2D matrix.
}
\value{
  A 2D matrix equivalent to the long-format data frame
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{matrix2longdf}
}
\examples{
test.df <- data.frame(H=c("HSV", "BVB", "HSV", "BVB"),
A=c("FCB", "S04", "S04", "FCB"),
score=c(3, 1, 1, 0))
longdf2matrix(test.df, row.col=1L, column.col=2L, value.col=3L)

data(Indometh)
longdf2matrix(Indometh, row.col="time", column.col="Subject",value.col="conc")
longdf2matrix(Indometh, row.col="Subject", column.col="time", value.col="conc")
}
