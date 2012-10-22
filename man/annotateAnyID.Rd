\name{annotateAnyID}
\alias{annotateAnyID}
\title{
  Annotate any identifiers so far they are GeneIDs, or GeneSymbols, or Probesets.
}
\description{
  This function is intended mostly for R-script builders who want to
  accept identifiers from users, which shall be annotated. It is a
  wrapper of three functions: \code{annotateProbesets},
  \code{annotateGeneIDs} and \code{annotateGeneSymbols}.
}
\usage{
annotateAnyID(ids, chiptype, orthologue = FALSE)
}
\arguments{
  \item{ids}{A vector of identifiers. They must be of the same type:
    GeneID, GeneSymbol or Probesets.}
  \item{chiptype}{A character string, can be any GTI chiptypes, or one
    of the two: 'GeneID' or 'GeneSymbol' (case insensitive).}
  \item{orthologue}{Logical, is orthologous mapping needed?}
}
\details{
  Input identifiers must be of the same type (GeneID, GeneSymbol or
  probesets). Mixed lists are not supported.
}
\value{
  A \code{data.frame} containing annotation information.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{annotateGeneIDs}},  \code{\link{annotateGeneSymbols}} and   \code{\link{annotateProbesets}}
}
\examples{
options(error=utils::recover)

annotateAnyID(ids=c(780, 5982, 3310, NA), chiptype="GeneID")

annotateAnyID(ids=c("DDR1", "RFC2", "HSPA6", "HSAP6"),
chiptype="GeneSymbol")

myprobes <- c("1000_at", "1004_at", "1002_f_at", "nonsense_at")
annotateProbesets(myprobes, chiptype="HG_U95A")
options(error=NULL)
}

