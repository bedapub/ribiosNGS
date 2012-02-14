\name{readGct}
\alias{checkfile}
\alias{readGct}
\alias{readGctNative}
\alias{makeTempGct}
\alias{makeGctLines}
\title{
  Importing GCT files
}
\description{
  The function \code{readGct} demonstrates how to use the
  \code{linestream} module of the BIOS system to parse plain text
  files. The function imports expression table stored in a GCT file as
  a \code{matrix} object in R, optionally with descriptions of features.

  The \code{readGctNative} function provides similar functionality, with
  the exception that it is implemented in native R codes. Although we do
  not claim it is the fatest R implementation, it is without question
  one of the simplest implementations: including integrity check
  (similar as the BIOS-counterpart).

  When comparing to the R-implementation, \code{readGct} using BIOS had
  dramatical performance advantages: for an expression matrix with 22000
  genes and 200 samples, it was finished less than one tenth of the time needed
  by \code{readGctNative}.

  \code{checkfile} is an auxiliary function for \code{readGct} to expand
  relative paths. \code{makeGctLines} and \code{makeTempGct} are helper
  functions to produce and save a random Gct file of given dimensions, respectively.
}
\usage{
readGct(gct.file, keep.desc = TRUE)
readGctNative(gct.file, keep.desc = TRUE)
checkfile(filename)
makeGctLines(nrow=100, ncol=100)
makeTempGct(nrow=100, ncol=100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gct.file}{A GCT format file}
  \item{keep.desc}{Logical, if set as \code{TRUE}, feature descriptions
    will be returned as the attribute (\code{desc}) of the matrix.}
  \item{filename}{Character, a file name to be checked. In case a
    relative file path is found, it is expanded.}
  \item{nrow}{Row number of the expression matrix}
  \item{ncol}{Column number of the expression matrix}
}
\details{
  \code{makeGctLines} produces a character vector of a GCT file, with
  random expression values. \code{makeTempGct} internally calls
  \code{makeGctLines}, and writes the content into a temporary file.
}
\value{
  Both \code{readGct} and \code{readGctNative} returns a matrix,
  optionally with feature descriptions.

  \code{makeGctLines} returns all lines of a GCT file as a character
  vector. \code{makeTempGct} returns the file name of the temporary GCT
  file.
}
\references{
  GCT file format description \url{http://www.broadinstitute.org/cancer/software/genepattern/tutorial/gp_fileformats}.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>, with input from
  Clemens Broger
}
\examples{
gctTmp <- makeTempGct(nrow=2000, ncol=100)
system.time(readGct(gctTmp))
system.time(readGctNative(gctTmp))
file.remove(gctTmp)
}
