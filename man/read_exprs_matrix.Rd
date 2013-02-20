\name{read_exprs_matrix}
\alias{read_exprs_matrix}
\title{
  Read an expression matrix from file
}
\description{
  Read an expression matrix from file. The file is either a GCT format
  file, a tab-delimited file or a space-delimited file. 
}
\usage{
read_exprs_matrix(x)
}
\arguments{
  \item{x}{File name}
}
\details{
  An expression matrix of size MxN contains exprssion levels of M
  features in N samples. This function supports three commonly used file
  formats for expression levels: \code{GCT} format, \code{tab-delimited
    file} and \code{space-delimited file}.
}
\value{
  A \code{matrix}
}
\author{Jitao David Zhang <jitao_david.zhang@roche.com>}
\note{
  The function uses a very simple logic to guess whether the file is
  tab-delimited or space-delimited: it reads in the first n lines
  (currently \code{n=3}), and checks whether there is any tab character
  (\code{\\t}): if yes, the file is parsed as tab-delmited, otherwise as
  space-delimited. Therefore, a space-delimited file should not contain
  tabs in case it needs to be parsed.

  From ribiosIO version 1.0.2, this function supports duplicated row
  names.

  From ribiosIO version 1.0-21, this function supports matrix file in
  which the second column is not numeric. This can happen, for instance,
  if the user decides to include descriptions. If such descriptions are
  detected, they are stored in the attribute \dQuote{desc} so as to be
  later written into gct files.
}
\seealso{
  The function calls internally the \code{read_gct_matrix} function to
  parse GCT files.
}
\examples{
testfile.path <- system.file("extdata", package="ribiosIO")

## import gct
read_exprs_matrix(file.path(testfile.path,"test_read_exprs_matrix.gct"))

## import tab-separated file
read_exprs_matrix(file.path(testfile.path, "test_read_exprs_matrix.tsv"))

## import space-separated file
read_exprs_matrix(file.path(testfile.path, "test_read_exprs_matrix.txt"))

## import tab-separted file with descriptions
read_exprs_matrix(file.path(testfile.path, "test_read_exprs_matrix_desc.tsv"))
}

