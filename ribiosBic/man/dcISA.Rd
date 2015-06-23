\name{dcISA}
\alias{dcISA}
\title{
  Run ISA in the _divide and conquer_ fashion
}
\description{
  Given class labels of samples, this function applies ISA on samples of
  each class separatedly.
}
\usage{
dcISA(data, group, globalflist = filterfun(function(x) IQR(x) > 0.5), flist=NA, uniqueEntrez = TRUE, thr.gene = seq(2, 4, by = 0.5), thr.cond = seq(1, 3, by = 0.5), no.seeds = 100)
}
\arguments{
  \item{data}{The input, an \linkS4class{ExpressionSet} object}
  \item{group}{Class labels of samples, a vector of factors. Vector of
    other types will be cast into factors.}
  \item{globalflist}{A \sQuote{list} of filter functions to apply to all the
    samples of the expression set. If local filtering within each class
    is desired, use the \code{flist} option.}
  \item{flist}{A \sQuote{list} of filter functions to apply to the
    expression array in each group. If global filtering is desired, use the
    \code{globalflist} option.}
  \item{uniqueEntrez}{Logical scalar, whether to filter the input
    expression set to keep exactly one probe for each Entrez gene, which
    has the largest between-sample variability. Probes that are not
    mapped to an Entrez GeneID are dropped.}
  \item{thr.gene}{Numeric vector, the threshold parameters of the ISA
    for features. All combinations of \sQuote{thr.gene} and
    \sQuote{thr.cond} will be used to run ISA.}
  \item{thr.cond}{Numeric vector, the threshold parameters of the ISA
    for samples. All combinations of \sQuote{thr.gene} and
    \sQuote{thr.cond} will be used to run ISA.}
  \item{no.seeds}{Number of seeds to start when running ISA}
}
\details{
  The ISA is run independently on each subset of samples, defined by the
  \code{group} variable.
}
\value{
  An object of \code{dcISAModules}, contains the class information as
  well as the modules identified in each subset of samples.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{ISA}}
}
\examples{
library(ALL)
data(ALL)
subtype <- gsub("^(B|T).*", "\\1", ALL$BT)
dcModules <- dcISA(ALL, group=subtype, thr.gene=2.7, thr.cond=1.4)
## modules <- ISA(ALL, thr.gene=2.7, thr.cond=1.4)
}
