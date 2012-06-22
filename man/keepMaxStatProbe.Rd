\name{keepMaxStatProbe}
\alias{keepMaxStatProbe}
\title{
  Filter multiple probesets matching to the same gene by keeping the one
  with the maximum statistic (by default the variance).
}
\description{
  The function filters features (commonly probesets) in an
  \code{ExpressionSet} object. It does not affect genes with only one
  feature present, or genes without an valid annotation (see details
  below). For genes with multiple probesets, the function calculates the
  statistic of each probeset across all samples and filter probesets by
  only keeping the one with the maximum of variance. Thereby an
  \code{ExpressionSet} returned by the function has only one probeset
  matching each gene.
}
\usage{
keepMaxStatProbe(eset, probe.index.name, keepNAprobes = TRUE,
stat=function(x) mean(x, na.rm=TRUE), ...)
}
\arguments{
  \item{eset}{An \code{ExpressionSet} }
  \item{probe.index.name}{The column name of the \code{fData(eset)} data
  matrix, used as the index of gene to determine which features are
  matched to the same gene.}
  \item{keepNAprobes}{Logical, determines whether genes without an valid
    index name should kept or left out. See details below.}
  \item{stat}{Function or character, a function (or the name referring
    to it) which takes a vector of numerical values, and returns one
    value as the statistic, e.g. \code{sd} for standard deviations.}
  \item{...}{Parameters passed to the \code{stat} function. One of the
    most frequent used option might be \code{na.rm=TRUE}, see details and examples.}
}
\details{
  Names of probesets are determined by the \code{featureNames(eset)}
  function.

  The column of \code{probe.index.name} in the \code{fData(eset)}
  data.frame determines the index of genes, for example the Entrez
  GeneID, to which probesets are matched. Those genes without a valid
  index, whose index is either an empty string or \code{NA}, can be set
  to be left out by \code{keepNAprobes=FALSE}. If the option is set as
  \code{TRUE}, then these genes are kept in the returning object.

  The \code{stat} function should only return one statistic, most favorably
  not NA, by taking a vector of numerical values. Most statistics can be
  calculated in a robust way by setting \code{na.rm=TRUE}. This option
  should be always used whenver possible. Otherwise when there is one or
  more missing value of a probeset, its statistic will probably be
  \code{NA} and this will lead to discard the probeset. Even worse, when
  all probesets matching to a gene have \code{NA}s, the gene will be
  totally filtered out, which is usually not desired. Therefore, set
  \code{na.rm=TRUE} through the \code{...} option (see examples below)
  whenever possible.
}
\value{
  An filtered \code{ExpressionSet}.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\note{
  Note that when the statistics of two or more probesets tie (having the
  same value), the probeset chosed could be random (the probeset with
  its name ranked first when multiple names are converted into a factor vector).
}

\examples{
example.mat <- matrix(c(1,1,3,4, 2,2,3,3, 4,5,6,7, 7,8,9,10), ncol=4, byrow=TRUE)
example.eset <- new("ExpressionSet", exprs=example.mat)

featureNames(example.eset) <- c("1a","1b","2","3")
fData(example.eset)$geneid <- c(1,1,2,3)

## keep probesets with the maximal variance
example.sd <- keepMaxStatProbe(example.eset, probe.index.name="geneid", stat=sd)
featureNames(example.sd)

## keep probesets with the maximal Median Absolute Deviation (MAD)
example.mad <- keepMaxStatProbe(example.eset, probe.index.name="geneid", stat=mad)
featureNames(example.mad)

## keep probesets with the maximal mean value
example.mean <- keepMaxStatProbe(example.eset,
probe.index.name="geneid", stat=mean)
featureNames(example.mean)

## note that NA value may cause problems, it is a good practice to make
## the stat function _resist_ to NA
na.eset <- example.eset
exprs(na.eset)[1,1] <- NA

\dontrun{
## prone to error
na.mean <- keepMaxStatProbe(na.eset,
probe.index.name="geneid",stat=mean)
featureNames(na.mean)
## better
na.mean.narm <- keepMaxStatProbe(na.eset,
probe.index.name="geneid",na.rm=TRUE)
featureNames(na.mean.narm)
}

}
