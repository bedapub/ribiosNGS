\name{readGctCls}
\alias{readGct}
\alias{readCls}
\alias{readGctCls}
\title{
  Read ExpressionSet from Gct/Cls files
}
\description{
  As complementary functions to \code{writeGctCls}, \code{readGctCls}
  reads a pair of gct and cls files (with same base names) into an
  \code{ExpressionSet} object.

  The \code{readGctCls} function calls internally the \code{readGct} and
  \code{readCls} functions to read in two formats
  respeectively. \code{readGct} returns a barely annotated
  \code{ExpressionSet} object, and \code{readCls} returns a vector of
  levels encoding sample groups.
}
\usage{
readGctCls(file.base,gct.file,cls.file,add.fData.file, add.pData.file)
readGct(gct.file)
readCls(cls.file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file.base}{The full file name of gct/cls files without suffixe,
    if not in the current diretory, must contain the path (dirname) as
    well . For instance if it is set as \code{~/my/dir/input}, then the function seeks the file \code{~/my/dir/input.gct}
    and \code{~/my/dir/input.cls}.}
  \item{gct.file}{The name of the gct file (only valid when file.base is missing).}
  \item{cls.file}{The name of the cls file (only valid when file.base is missing).}
  \item{add.fData.file}{Optional, file of additional feature data, see details.}
  \item{add.pData.file}{Optional, file of additional phenotype (sample) data, see
    details.}
}
\details{
  Since gct/cls contains only one property of features and samples each
  (Description in the gct file as well as sample groups/levels in the
  cls file), \code{readGctCls} allows users to provide additional
  fData/pData files. They should be tab-delimited files, with first
  column machting exactly the names of features or samples. They must be
  within the path specified by the \code{path} option, namely in the
  same directory of gls/cls files.sample

  See example below.
}
\value{
  An \code{ExpressionSet} object. The \code{Description} column in the
  gct file is encoded in the \code{desc} column in the featureData of
  the resulting object. The sample groups in the cls file is encoded in
  the \code{cls} column in the phenoData.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\note{
  The \code{readGct} function is a wrapper of the
  \link[ribiosIO]{read_gct_matrix} function in the \code{ribiosIO}
  package, which makes up the GCT matrix into an \code{ExpressionSet}
  object.
}
\seealso{
  \code{\link{writeGctCls}}. See \code{\link[ribiosIO]{read_gct_matrix}}
  for underlying C code to import GCT files.
}
\examples{
idir <- system.file("extdata", package="ribiosExpression")

sample.eset <- readGctCls(file.base=file.path(idir, "test"))

ext.eset <- readGctCls(file.base=file.path(idir, "test"),
add.fData.file=file.path(idir, "test.add.fData.txt"),
add.pData.file=file.path(idir, "test.add.pData.txt"))

stopifnot(identical(exprs(sample.eset), exprs(ext.eset)))

## try to compare pData(sample.eset) with pData(ext.eset), and similarly
## fData(sample.eset) with fData(ext.eset)
}
