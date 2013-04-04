\name{RIFscore}
\alias{RIFscore}
\title{
Calculate Regulatory Impact Factor (RIF)
}
\description{
Calculate RIF scores. For the definition of scores see the reference below.
}
\usage{
RIFscore(matrix, reg.ind, de.ind=1:nrow(matrix), fac, coefs,
method=c("spearman", "pearson", "kendall"), permutation=NULL)
}
\arguments{
  \item{matrix}{An expression matrix}
  \item{reg.ind}{Indices of regulatory genes (e.g. Transcription
    Facotrs)}
  \item{de.ind}{Indices of differential expressed features. All genes
    are used if not given}
  \item{fac}{Sample factors. Must be of the same length as the column
    number of the matrix}
  \item{coefs}{In case of more than two sample groups, \code{coefs}
    specifies which two contrasts are of interest. Optional. Ff
    provided, it must be a character vector of length two.}
  \item{method}{Method to calculate coefficient coefficients.}
  \item{permutation}{If set as an integer \code{N}, sample labels will be
    randomly permutated \code{N} times and empirical p values are reported.}
}
\details{
  RIF (regulatory impact factor) scores measure the impacts of
  transcription factors. Two variants of the RIF score (RIF1 and RIF2)
  are reported. TFs with higher RIF1 are consitently highly
  differentially co-expressed with the abundant differentially expressed
  genes; TFs with higher RIF2 have the most altered ability to predict
  the abundance of differentially expressed genes.
}
\value{
  \code{RIFscore} returns a \code{data.frame} with following columns:
  \enumerate{
    \item Index: Indices of TFs
    \item RIF1: RIF1 scores
    \item RIF2: RIF2 scores
    \item RIF1.rank: Rank of RIF1 scores (ascending)
    \item RIF2.rank: Rank of RIF2 scores (ascending)
    \item RIF1.pos.p: empirical p value of RIF1 score larger than the
    statistic of permutating samples
    (\code{NA} when \code{permutation=NULL})
    \item RIF1.neg.p: empirical p value of RIF1 score less than the
    statistic of permutating samples
    (\code{NA} when \code{permutation=NULL})
    \item RIF2.pos.p: similar to \code{RIF1.pos.p}
    \item RIF2.neg.p: similar to \code{RIF1.neg.p}
  }
}
\references{
  Reverter \emph{et al.} Regulatory impact factors: unraveling the transcriptional regulation
  of complex traits from expression data. Bioinformatics, 2010(26), 896--904.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
if(require("Biobase")) {
  data(sample.ExpressionSet)
  type.rif <- RIFscore(log2(exprs(sample.ExpressionSet)+100),
reg.ind=sample(1:nrow(sample.ExpressionSet), 25), fac=sample.ExpressionSet$type)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}