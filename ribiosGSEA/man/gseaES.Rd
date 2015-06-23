\name{gseaResES}
\alias{gseaResQvalue}
\alias{gseaResES}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Read GSEA statistic for pathway fingerprinting
}
\description{
  Read GSEA statistics (log-transformed q-value [q], Enrichment Score [ES], or
  normalized Enrichement Score [NES]) to profile pathway activitities.
}
\usage{
gseaResQvalue(file, threshold = 1e-04, log = FALSE, posLog = FALSE)
gseaResES(file, normalized = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{GSEA output tab-delimited file, usually with the file name
    \sQuote{gsea\_report\_for.*\_pos\_.*\\.xls} or
    \sQuote{gsea\_report\_for.*\_neg\_.*\\.xls}. Located in GSEA output
    directory.}
  \item{threshold}{Valid for q value: what is the minimum threshold of
    q-value (FDR)? It can be set to the number of permutation tests divided
    by \code{1}. By default \code{1/10000}}
  \item{log}{Valid for q value: whether the FDR q value should be
    transformed by base-10 (log10) logarithm. By default \code{FALSE}}
  \item{posLog}{Valid for q value: whether the logged FDR q value should be
    negated to get positive value.This is useful when the sign of
    \code{q} is used to distinguish between positive and negative
    enriched pathways. By default \code{FALSE}.}  
  \item{normalized}{Valid for enrichment score: if set to \code{TRUE},
    normalized enrichment score (nes) will be returned instead of
    (es). By default set to \code{FALSE}}
}
\details{
  In many cases we want to extract pathway signatures from a set of
  experiments. Both \code{gseaResQvalue} and \code{gseaES} can read GSEA
  output files and extract desired statistic: q-value, ES or NES.

  See the GSEA document for definitions of the three values. For
  comparing a few conditions to another, we recommend using \emph{q-value}. For
  large-scale comparisons between pathways (or other gene signatures),
  we have found \emph{ES} very useful. It is adviced to choose proper
  statistic to extract pathway signatures only when you are sure of the
  aim. Using any statistic without good reasoning may as always lead to
  wrong intepretations of the data.

  These functions are usually not directly called by end-users. See
  \code{\link{gseaFingerprint}} and \code{link{gseaFingerprintMatrix}} instead.
}
\value{
  A \code{data.frame} with two columns: \code{name} and
  \code{value}. The column \code{name} contains gene signatures
  (e.g. pathways), and \code{value} contains the statistic.
}
\references{
  GSEA documentation \url{http://www.broadinstitute.org/gsea/doc/GSEAUserGuideFrame.html}
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>, with input from
  Martin Ebeling, Laura Badi and Isabelle Wells.
}
\seealso{
  End-users will probably find \code{\link{gseaFingerprint}} and
  \code{link{gseaFingerprintMatrix}} more useful, since they operate on
  the level of GSEA result directories, instead of single output
  tab-delimited files.
}
\examples{
gsea.dir <- system.file(package="ribiosGSEA",
"extdata/gseaDirs/HumanInVitroLiver_VitaminA_24h_High")
gsea.file <- file.path(gsea.dir,
"gsea_report_for_na_neg_1336489010730.xls")

gsea.q <- gseaResQvalue(gsea.file)
gsea.logq <- gseaResQvalue(gsea.file, log=TRUE)
gsea.logq.pos <- gseaResQvalue(gsea.file, log=TRUE, posLog=TRUE)

gsea.es <- gseaResES(gsea.file)
gsea.nes <- gseaResES(gsea.file, normalized=TRUE)
}
