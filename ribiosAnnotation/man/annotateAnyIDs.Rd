\name{annotateAnyIDs}
\alias{annotateAnyIDs}
\title{
  Annotate any identifiers
}
\description{
  This annotates any identifies that can be recognized by GTI.
}
\usage{
annotateAnyIDs(ids,orthologue = FALSE, multiOrth = FALSE)
}
\arguments{
  \item{ids}{A vector of identifiers. They must be of the same type:
    GeneID, GeneSymbol or Probesets.}
  \item{orthologue}{Logical, is orthologous mapping needed?}
  \item{multiOrth}{Logical, is more t han one orthologs allowed}

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

annotateAnyIDs(ids=c(780, 5982, 3310, NA))

annotateAnyIDs(ids=c("DDR1", "RFC2", "HSPA6", "HSAP6"))

myprobes <- c("1000_at", "1004_at", "1002_f_at", "nonsense_at")
annotateProbesets(myprobes, chiptype="HG_U95A")

annotateAnyIDs(ids=c("P38398", "Q8NDF8"))
options(error=NULL)
}

