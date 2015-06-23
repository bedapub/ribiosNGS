\name{gseaScore}
\alias{gseaScore}
\alias{gseaScores}
\title{
  Extract scores from GSEA results
}
\description{
  One way to score GSEA results is to multiple the absolute value of
  log10 transformed p-values (nominal p-value, FDR, or FWER) with the
  sign of the enrichment scores. This score is intuitive since it
  combines statistical significance and the sign of regulation.
}
\usage{
gseaScore(x, type = c("fdr", "p", "fwer"))
gseaScores(..., names=NULL, type = c("fdr", "p", "fwer"))
}
\arguments{
  \item{x}{An \code{annoGseaRes} object}
  \item{type}{Character string, the type of p-value used to calculate
    the score.}
  \item{...}{Objects of \code{annoGseaRes} to be compared}
  \item{names}{Character strings, names given to the result score
    sets. See examples below.}
}
\details{
  \code{gseaScores} takes care of the situation where some gene sets are
  missing in one or more conditions. 
}
\value{
  \code{gseaScore} returns a double vector of scores with gene set
  names.

  \code{gseaScores} returns a data frame of scores, with gene set names
  as row names. 
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{gseaNP}}, \code{\link{gseaFDR}}, \code{\link{gseaFWER}} to
  get p-values.

  \code{\link{parseGSEAdir}}.
}
