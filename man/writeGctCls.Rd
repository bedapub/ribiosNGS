\name{writeGctCls}
\alias{writeGct}
\alias{writeCls}
\alias{writeGctCls}
\title{Export ExpressionSet as Gct/Cls files}
\description{
  Gct/Cls file formats are required by the Gene Set Enrichment Analysis
  (GSEA) tool. Functions \code{writeGct} and \code{writeCls} exports
  file of two formats respectively, and \code{writeGctCls} calls the two
  function internally to write two files.
}
\usage{
writeGct(eset, file=stdout(), feature.name.col, feature.desc.col)
writeCls(eset, file=stdout(), sample.group.col)
writeGctCls(eset, file.base, feature.name.col, sample.group.col,
write.add.fData.file=TRUE, write.add.pData.file=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{eset}{An object of the \code{eSet} class, for example an
    \code{ExpressionSet} object}
  \item{file}{Name of the Gct/Cls file}
  \item{file.base}{For writeGctCls, the base name of the two files: the
    suffix (.gct and .cls) will be appended}
  \item{feature.desc.col}{Integer or character, indicating which column
    of the featureData should be used as feature names; if missing,
    results of the \code{featureNames} function will be used as
    identifiers in the Gct file. See details.}
  \item{feature.name.col}{Integer or character, indicating which column
    of the featureData should be used as feature descriptions. If the
    value is missing, the Description column of the Gct file will be
    left blank}
  \item{sample.group.col}{Integer, character or a factor vector of the
    same length as the sample number, indicating classes (groups) of
    samples. See details.}
  \item{write.add.fData.file}{Logical, whether additional featureData
    should be written into a file named \code{${file.base}.add.fData.txt}}
  \item{write.add.pData.file}{Logical, whether additional phenoData
    should be written into a file named \code{${file.base}.add.pData.txt}}
}
\details{
  The \code{feature.name.col} option specifies what identifiers should
  be used for features (probesets). When the value is missing,
  \code{featureNames} is called to provide feature identifiers.

  In contrast, the \code{sample.group.col} cannot be missing: since cls
  files encode groups (classes) of samples, and if
  \code{sample.group.col} was missing, it is usually impossible to get
  class information from \code{sampleNames}.

  Internally \code{writeCls} calls \code{\link{dfFactor}} function to
  determine factor of samples. Therefore \code{sample.group.col} is
  to a certain degree generic: it can be a character string or integer
  index of the \code{pData(eset)} data matrix, or a factor vector of the
  same length as \code{ncol(eset)}.
}
\value{
  Functions are used for their side effects.
}
\references{
  \url{http://www.broadinstitute.org/gsea/doc/GSEAUserGuideTEXT.htm}
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  See \code{\link{dfFactor}} for possible values of the \code{sample.group.col} option.

  See \code{\link{readGctCls}} for importing functions.
}
\examples{
data(sample.ExpressionSet)
writeGct(sample.ExpressionSet[1:5, 1:4], file=stdout())
writeCls(sample.ExpressionSet, file=stdout(), sample.group.col="type")

tmpfile <- tempfile()
writeGctCls(sample.ExpressionSet, file.base=tmpfile, sample.group.col="type")
readLines(paste(tmpfile, ".cls",sep=""))
unlink(c(paste(tmpfile, ".cls", sep=""), paste(tmpfile, ".gct", sep="")))
}
