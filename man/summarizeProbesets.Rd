\name{summarizeProbesets}
\alias{summarizeProbesets}
\title{Summarize probesets belonging to the same gene}
\description{
  The \code{summarizeRows} function summarizes (collapses) rows of a
  numeric matrix by calculating summarizing statistics of rows that
  belong to the same factor level.
}
\usage{
summarizeProbesets(eset, index.name, fun = mean, keep.nonindex = FALSE,
keep.featureNames = FALSE, ...)
}
\arguments{
  \item{eset}{An \code{ExpressionSet} object}
  \item{index.name}{Charcter, one column name in the \code{eset}, indicating
    the index of probesets, probably the column holding the GeneID.}
  \item{fun}{Function or character, the function used to summarize probes, \code{mean} by
    default. Other possibilities include \code{median}.}
  \item{keep.nonindex}{Logical, whether probesets without valid indices
    should be kept or not.}
  \item{keep.featureNames}{Logical, whether the featureNames of the input
    object should be kept whenever possible. When multiple probesets are
    summarized into one value representing, for example, one gene (by
    GeneID), one arbitrary probeset is used to name the value when this option is set
    to \code{TRUE}. Otherwise the GeneID will be used as the name of the
    value. In case no summary was possible, for instance the index is
    \code{NA}, old feature names are kept any way.}
  \item{...}{Futher parameters passed to the function}
}
\details{
  \code{summarizeRows} is called internally by \code{summarizeProbesets}
  to collapse probesets that belong to one index (e.g. GeneID).

  The action of this function is univariate: namely the \code{fun} is
  applied to all probesets on each sample independently. For example, if
  \code{fun} is \code{mean}, the average value of mutliple probesets is
  taken for each sample. With this function, there is no way to distinguish probesets on
  their expression profiles (for instance: find the probeset with the
  maximum average signal).
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
library(ribiosUtils)
ribios.meanMat <- summarizeRows(exprs(ribios.ExpressionSet),
fData(ribios.ExpressionSet)$GeneID, mean)
stopifnot(identical(exprs(ribios.mean), ribios.meanMat))

## keep old featureNames
ribios.inval.mean.old <- summarizeProbesets(ribios.ExpressionSet,
index.name="GeneID", fun=mean, keep.nonindex=TRUE, keep.featureNames=TRUE)
}
