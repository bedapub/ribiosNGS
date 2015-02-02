\name{summarizeSamples}
\alias{summarizeSamples}
\alias{poolReplicates}
\alias{avgReplicates}
\title{
  Summarize samples by applying the function to sample subsets
}
\description{
  The function takes an \code{eSet} object and a factor of the same
  length as the object, and summarizes samples of the same factor level
  by applying the function.

}
\usage{
summarizeSamples(eset, indSamples = eset$SAMPLEID, removeInvarCols = TRUE, fun = sum, ...)
}
\arguments{
  \item{eset}{An \code{eSet} object.}
  \item{indSamples}{A factor of the same length as the sample number of
    the object}
  \item{removeInvarCols}{Logical, whether invariant columns of the
    resulting \code{eSet} pheno data should be removed or not. This is
    useful in case there are technical replicates.}
  \item{fun}{The function to be applied to summarize samples}
  \item{\dots}{Other parameters passed to the function}
}
\details{
  \code{poolReplicates} and \code{avgReplicates} are two specific form
  of the more generic \code{summarizeSamples} function: they take sum
  and average of replicates given by the factor, respectively.
}
\value{
  A \code{eSet} object.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  The function calls \code{\link[ribiosUtils]{summarizeColumns}}
  internally.

  Also see \code{\link{summarizeProbesets}}.
}
\examples{
data(ribios.ExpressionSet, package="ribiosExpression")
index <- factor(c(gl(12,2), 13, 14))

(ss.eset1 <- summarizeSamples(ribios.ExpressionSet, index))

(ss.eset2 <- summarizeSamples(ribios.ExpressionSet, index, fun=mean,
na.rm=TRUE))
## equivalently 
(ss.eset2 <- poolReplicates(ribios.ExpressionSet, index))

(ss.eset3 <- avgReplicates(ribios.ExpressionSet, index))
}

