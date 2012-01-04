\name{readExprsMatrix}
\alias{readExprsMatrix}
\title{Read an expression matrix into an ExpressionSet object}
\description{The function reads in an expression matrix into an
  ExpressionSet object. The expression matrix should be saved in the
  file format supported by the \code{\link{read_exprs_matrix}} function:
  currently supported formats include tab-delimited file and gct files.}
\usage{
readExprsMatrix(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A file containing an expression matrix}
}
\details{The function is a wrapper of the \code{\link{read_exprs_matrix}}
  function in the \code{ribiosIO} package. The difference is it returns
  a valid \code{ExpressionSet} object instead of a primitive matrix.}
\value{
  An \code{ExpressionSet} object holding the expression matrix. Both
  pData and fData are empty except for the feature/sample names recorded
  in the expression matrix.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{read_exprs_matrix}} in the \code{ribiosIO} package.
}
\examples{
idir <- system.file("extdata", package="ribiosExpression")
myeset <- readExprsMatrix(file.path(idir, "sample_eset_exprs.txt"))
myeset2 <- readExprsMatrix(file.path(idir, "test.gct"))
}
