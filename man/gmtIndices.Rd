\name{gmtIndices}
\alias{gmtIndices}
\title{
  Convert a GMT list to indices of a given vector of symbols
}
\description{
  Some functions require that gene sets are provided in the form of
  integer indices of an ordered vector of genes, for instance the row
  indices of an \code{ExpressionSet} object. This function identifies
  indices of genes in the sets stored in a GMT list.
}
\usage{
gmtIndices(gmtlist, symbols)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gmtlist}{A GMT list, probably read in from a plain file by the
    \code{read_gmt_list} function in the \code{ribiosIO} package.}
  \item{symbols}{A vector of gene symbols, with which the indices are
    determined. See example.}
}
\value{
  A list of integers, with names of the input list. 
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link[ribiosIO]{read_gmt_list}} of the \code{ribiosIO} package.
}
\examples{
foo.list <- list(list(name="Set1", description="", genes=c("HSV", "FCB","BVB")),
                 list(name="Set2", description="", genes=c("HSV","FCN")),
                 list(name="Set3", description="", genes=c("HAN","FCN", "FSC")))
symbols <- c("HSV", "BVB", "HAN", "FCN", "FCB")
gmtIndices(foo.list, symbols)
}

