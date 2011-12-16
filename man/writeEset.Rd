\name{writeEset}
\alias{readEset}
\alias{writeEset}
\title{
  Import and export an ExpressionSet object as tab-delimited files
}
\description{
  Two functions, \code{writeEset} and \code{readEset}, import and export
  an \code{ExpressionSet} object as tab-delimited files
  respectively. See details below for advantages and limitations.
}
\usage{
writeEset(eset, exprs.file, fData.file, pData.file)
readEset(exprs.file, fData.file, pData.file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{eset}{Required for \code{writeEset}, an \code{ExpressionSet}
    object to be exported.}
  \item{exprs.file}{Required, character string, full name of the file containing the expression
    matrix.}
  \item{fData.file}{Optional, character string, full name of the file containing feature
    annotations. \code{NULL} is handled specially: it will cause no
    reading or writing of the feature annotation data.}
  \item{pData.file}{Optional, character string, full name of the file
    containing sample annotations. \code{NULL} is handled specially: it
    will cause no reading or writing of the sample annotation data.}
}
\details{
  \code{readEset} and \code{writeEset} provide a lightweighted mechanism
  to import/export essential information from/to plain text files. They
  can use up to three tab-delimited files to store information of an
  \code{ExpressionSet} object: a file holding the expression matrix as
  returned by the \code{\link{exprs}} function (\code{exprs.file}), a
  file containing feature annotations as returned by the \code{\link{fData}}
  function (\code{fData.file}), and finally a file containing sample
  annotations, as returned by \code{pData} (\code{pData.file}).

  All three files are saved as tab-delimited, quoted plain files with
  both row and column names. They can be readily read in by the
  \code{read.table} function with default parameters.

  In both functions, \code{fData.file} and \code{pData.file} are
  optional. Leaving them missing or settign their values to \code{NULL}
  will prevent exporting/importing annotations.

  One limitation of these functions is that they only support the
  export/import of \strong{one} expression matrix from one
  \code{ExpressionSet}. Although an \code{ExpressionSet} can hold more
  than one matrices other than the one known as \code{exprs}, they are
  not handled now by \code{writeEset} or \code{readEset}. If such an
  \code{ExprssionSet} object is first written in plain files, and then
  read back as an \code{ExpressionSet}, matrices other than the one
  accessible by \code{exprs} will be discarded.

  Similarly, other pieces of information saved in an \code{ExpressionSet}, e.g. annotations or
  experimental data, are lost as well after a cycle of exporting and
  subsequent importing. If keeping these information is important for
  you, other functions should be considered instead of \code{readEset}
  and \code{writeEset}, for instance to save an image in a binary file
  with the \code{\link{save}} function.
}
\value{
  \code{readEset} returns an \code{ExpressionSet} object from plain
  files.

  \code{writeEset} is used for its side effects (writing files).
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\note{
  \code{readEset} will stop if the fData.file or pData.file does not
  look like a valid annotation file, by checking they have the same
  dimension as suggested by the expression matrix, and matching the
  feature/sample names with those stored in the expression matrix file.
}
\seealso{
  See \code{\link{readGctCls}} and \code{\link{writeGctCls}} for
  importing/exporting functions for files in gct/cls formats.
}
\examples{
sysdir <- system.file("extdata", package="ribiosExpression")
sysexp <- file.path(sysdir, "sample_eset_exprs.txt")
sysfd <- file.path(sysdir, "sample_eset_fdata.txt")
syspd <- file.path(sysdir, "sample_eset_pdata.txt")

sys.eset <- readEset(exprs.file=sysexp,
                     fData.file=sysfd,
                     pData.file=syspd)
sys.eset
}
