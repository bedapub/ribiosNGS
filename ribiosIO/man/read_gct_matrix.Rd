\name{read_gct_matrix}
\alias{read_gct_matrix}
\alias{read_gct}
\alias{read_gctstr_matrix}
\title{
  Calling C routine to read GCT file into a matrix
}
\description{
  The function \code{read_gct_matrix} calls the C routine
  \code{read_gct} to read GCT file into a matrix.

  The function \code{read_gctstr_matrix} calls the C rountine as well,
  to parse a character string in the GCT file format into a matrix.
}
\usage{
read_gct_matrix(gct.file, keep.desc=TRUE)
read_gctstr_matrix(string, keep.desc=TRUE)
}
\arguments{
  \item{gct.file}{Character, name of a gct-format file}
  \item{string}{Character string, a character string in the GCT-file format}
  \item{keep.desc}{Logical, whether the description of features should
    be returned as an attribute of the matrix}
}
\details{
  Normal R users should consider using the \code{readGct} function in
  the \code{ribiosExpression} package. While \code{read_gct_matrix}
  reads in GCT into matrix, which is a basic data structure of \code{R},
  the \code{readGct} function calls \code{read_gct_matrix} and fill the
  matrix into an \code{ExpressionSet} object.
}
\value{
  An matrix, optionally with feature descriptions as an attribute
  (\code{desc}) when \code{keep.desc} is set to \code{TRUE}.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \link[ribiosExpression]{readGct} in the \code{ribiosExpression} package.
}
\examples{
idir <- system.file("extdata", package="ribiosIO")
sample.gct.file <- file.path(idir, "test.gct")

test.mat <- read_gct_matrix(sample.gct.file, keep.desc=TRUE)
test.simmat <- read_gct_matrix(sample.gct.file, keep.desc=FALSE)

sample.gct.string <- paste(readLines(sample.gct.file),collapse="\n")
teststr.mat <- read_gctstr_matrix(sample.gct.string, keep.desc=TRUE)
}
