\name{summarizeProbesets}
\alias{summarizeProbesets}
\title{Summarize probesets belonging to the same gene}
\description{
  The \code{summarizeRows} function summarizes (collapses) rows of a
  numeric matrix by calculating summarizing statistics of rows that
  belong to the same factor level.
}
\usage{
summarizeProbesets(eset, index.name, fun = mean, keep.nonindex = FALSE)
}
\arguments{
  \item{eset}{An \code{ExpressionSet} object}
  \item{index.name}{Charcter, one column name in the \code{eset}, indicating
    the index of probesets, probably the column holding the GeneID.}
  \item{fun}{Function or character, the function used to summarize probes, \code{mean} by
    default. Other possibilities include \code{median}.}
  \item{keep.nonindex}{Logical, whether probesets without valid indices
    should be kept or not.}
}
\details{
  \code{summarizeRows} is called internally by \code{summarizeProbesets}
  to collapse probesets that belong to one index (e.g. GeneID).
}
\value{
  An \code{ExpressionSet}, with probesets summarized by indices specified.
}
\author{Jitao David Zhang <jitao_david.zhang@roche.com>}
\seealso{\code{\link{summarizeRows}} in the \code{ribiosUtils} package.}
\examples{
data(ribios.ExpressionSet, package="ribiosExpression")
ribios.mean <- summarizeProbesets(ribios.ExpressionSet,
index.name="GeneID", fun=mean)
ribios.mean

ribios.inval.mean <- summarizeProbesets(ribios.ExpressionSet,
index.name="GeneID", fun=mean, keep.nonindex=TRUE)

## the underlying method
ribios.meanMat <- summarizeRows(exprs(ribios.ExpressionSet),
fData(ribios.ExpressionSet)$GeneID, mean)
stopifnot(identical(exprs(ribios.mean), ribios.meanMat))


}
