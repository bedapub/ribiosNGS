\name{kendallWmat}
\alias{kendallWmat}
\title{
  Use Kendall's W and graph theory to assign independent measurements into sub-groups by correlation
}
\description{
  Kendall's W, also known as Kendall's coefficient of concordance, is a
  non-parametric statistic developed to assess agreement among raters
  used in psychological or similar experimental settings. 

  In computational biology, the concept of associating features with
  similar patterns while keeping outliers can be useful in many
  cases. See the Details section for examples.

  This function implements the Kendall's W recursively with graph
  theory. It split grouped measurements into strongly associated
  sub-groups. See the Details section.
}
\usage{
kendallWmat(mat, row.factor, summary = c("none", "mean", "median",
"max.mean.sig", "max.var.sig"), na.rm = TRUE, alpha = 0.01)

}
\arguments{
  \item{mat}{A numeric matrix. It must contain at least 2 rows and 2 columns.}
  \item{row.factor}{A factor indicating groups of rows. In expression
    analysis, for instance, this can be GeneIDs indicating which
    probesets in rows belong to the same gene.}
  \item{summary}{Character, action to take once the sub-groups have been
  determined. \sQuote{none} indicates no action should be taken, the
  original data is returned with the information of
  sub-grouping. The option \sQuote{mean} (or \sQuote{median}) will take
  mean/median of features in each sub-group as result. On contrast,
  \code{max.mean.sig} or \code{max.var.sig} picks the feature of the
  largest mean signal or the largest variance in each sub-group as the
  representative. See details below.}
  \item{na.rm}{Logical, should those features whose \code{row.factor} are
    \code{NA} be left out? If set to \code{TRUE} (which is default), these unannotated features will be
  discarded from the results.}
  \item{alpha}{Nunmeric value, the significance level of the Kendall's W statistic. The
    larger the value, the more abbreviations from strong associations
    are allowed in sub-groups. Default is \code{0.01}.}
}
\details{
  We take a microarray experiment as an example to demonstrate how the
  function works. In microarrays, a gene is often represented by more than one probeset, and it is not
  rare that they do not all resemble the same expression
  pattern. Usually a \code{one gene-one value} relation is desired. Common practices including choosing the probeset with
  the highest average signal or the highest variance, as well as taking
  the mean/median value of all probesets mapped to one gene as the
  representative value.

  Kendall's W takes a very different approach. First it tries to judge whether
  multiple probesets of one gene are concordant. The concordance is
  determined by a non-parametric statistic closely related to Spearman
  correlation coefficient as well as Friedman's test. If all probesets
  are concordant, it means that their expression patterns are closely
  associated with each other. Any one of them, or the mean value, can be
  then used to represent the expression level of the gene.

  In cases where there is little concordance among probesets, we can
  take use of graph theory to iteratively search for sub-groups of
  probesets resemble each other's expression patterns. In the extreme
  case, each probeset can be different from the rest, and in this case
  the number of sub-groups will be equal to the number of probesets
  mapped to the gene. Such cases can appear, for instance, when
  each probeset was designed to target a different region of a
  transcript with splice variants. By using Kendall's W statistic with
  graph theory, the \code{kendallWmat} function can detect sub-groups with
  strongly correlated expression patterns, while keeping outliers on
  their own, therefore providing help for both conventional expression
  analysis and post-hoc analysis with the help of sequence analysis. See
  reference for examples on this application.

  We believe this approach is only useful for microarray, but can be
  also interesting for other applications like next-generation
  sequencing (NGS) or pathway/network analysis. For instance, in NGS
  experiments, this method can help to determine which splice variants
  of a transcript have similar expression patterns, and how different
  are other variants. In pathway analysis, when rows indicate gene
  expression values and \code{row.factor} indicate pathway membership,
  the result reveals which sub-networks are regulated associatively. 
}
\value{
  Currently a matrix with one attribute slot named \code{info}.
}
\references{
  The concept of Kendall's W was introduced in the seminal paper \emph{The problem of m rankings} by M.G. Kendall and B.B. Smith (The Annals
  of Mathematical Statistics, 1939). Schneider, Smith and Hansen
  developed the SCOREM algorithm combining this statistic with graph
  theory (\emph{SCOREM: statistical consolidation of redundant
    expression measures}, Nucleic Acids Research, 2011). This
  implementation is very much based on the SCOREM algorithm. The main
  changes are (1) the current implementation is more generic, applicable to native R
  data structures, therefore able to be applied in other scenario than
  microarray analysis (2) it takes not-annotated features into account as
  well and (3) it is possible to directly calculate summary statistics from sub-groups.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
## use a mock example
emat <- matrix(c(2,3,5,
                 8,9,2,
                 3,4,7,
                 0,2,1,
                 NA, 3, 1.2,
                 5, -3,4,
                 5,7,11), ncol=3, byrow=TRUE,
               dimnames=list(paste("row", 1:7, sep=""),NULL))
efac <- factor(c("a", "b", "c", NA, "b", "a", "a"),
               levels=letters[1:5])

print(emat)
kendallWmat(emat, efac, summary="none")
kendallWmat(emat, efac, summary="none", na.rm=FALSE)
kendallWmat(emat, efac, summary="mean")
kendallWmat(emat, efac, summary="mean", na.rm=FALSE)
kendallWmat(emat, efac, summary="median")
kendallWmat(emat, efac, summary="median", na.rm=FALSE)
kendallWmat(emat, efac, summary="max.mean.sig")
kendallWmat(emat, efac, summary="max.mean.sig", na.rm=FALSE)
kendallWmat(emat, efac, summary="max.var.sig")
kendallWmat(emat, efac, summary="max.var.sig", na.rm=TRUE)

## kendallW acts as an interface to matrix
kendallW(emat, efac, summary="none")

## kendallW acts as an interface to ExpressionSet
data(ribios.ExpressionSet)
kendallW(ribios.ExpressionSet, fData(ribios.ExpressionSet)$GeneID,
summary="none")
kendallW(ribios.ExpressionSet, fData(ribios.ExpressionSet)$GeneID, summary="mean")
}

