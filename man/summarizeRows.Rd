\name{summarizeRows}
\alias{apply1}
\alias{apply2}
\alias{summarizeRows}
\alias{summarizeCols}
\alias{summarizeColumns}
\title{Summarizing rows/columns by a factor}
\description{
  Apply a function to summarize rows/columns that assigned to the same
  level by a factor vector.
}
\usage{
summarizeRows(matrix, factor, fun = mean, ...)
summarizeColumns(matrix, factor, fun = mean, ...)
summarizeCols(matrix, factor, fun = mean, ...)
}
\arguments{
  \item{matrix}{A numeric matrix}
  \item{factor}{A vector of factors, either of the length of
    \code{nrow(matrix)} (for \code{summarizeRows}), or the length of
    \code{ncol(matrix)} (for \code{summarizeColumns}).}
  \item{fun}{A function or a name for a function, the summarizing
    function applied to rows/columns sharing the same level}
  \item{...}{Further parameters passed to the function}
}
\details{
  \code{NA} levels are neglected, and corresponding rows/columns will
  not contribute to the summarized matrix.

  \code{summarizeCols} is synonymous to \code{summarizeColumns}
}
\value{
  A matrix, the dimension will be determined by the number of levels of
  the factor vector.
}
\author{Jitao David Zhang <jitao_david.zhang@roche.com>}
\examples{
my.matrix <- matrix(1:25, nrow=5)
print(my.matrix)

my.factor <- factor(c("A", "B", "A", "C", "B"))
summarizeRows(matrix=my.matrix, factor=my.factor, fun=mean)
summarizeRows(matrix=my.matrix, factor=my.factor, fun=prod)
summarizeColumns(matrix=my.matrix, factor=my.factor, fun=mean)
summarizeColumns(matrix=my.matrix, factor=my.factor, fun=prod)

## NA values in factor
my.na.factor <- factor(c("A", "B", "A", "C", NA))
summarizeRows(matrix=my.matrix, factor=my.na.factor, fun=mean)
summarizeRows(matrix=my.matrix, factor=my.na.factor, fun=prod)
summarizeColumns(matrix=my.matrix, factor=my.na.factor, fun=mean)
summarizeColumns(matrix=my.matrix, factor=my.na.factor, fun=prod)
}

