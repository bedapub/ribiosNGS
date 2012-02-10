\name{annotateGeneIDs}
\alias{annotateGeneIDs}
\alias{annotateGeneSymbols}
\title{
  Annotate GeneIDs or GeneSymbols by a GTI chip annotation 
}
\description{
  Annotate GeneIDs or GeneSymbols, similarly to the way of annotating
  probesets, with a chiptype supported by GTI. 
}
\usage{
annotateGeneIDs(ids, chip = "HG-U133_PLUS_2", keepProbeID = TRUE)
annotateGeneSymbols(ids, chip = "HG-U133_PLUS_2", keepProbeID = TRUE)
}
\arguments{
  \item{ids}{Character vector, GeneIDs or GeneSymbols of query. It can contain \code{NA} or \code{NULL}}
  \item{chip}{Character, chiptype supported by GTI (use \code{gtiChiptypes}), from the annotation of which
    GeneID/GeneSymbols are mapped.}
  \item{keepProbeID}{Logical. If set to \code{TRUE}, the \code{ProbeID}
    column will contain probesets matched to GeneIDs/GeneSymbols in the
    given chip, otherwise the column is overwritten by
    GeneIDs/GeneSymbols. Note that in the former case, only one
    arbitrary probeset will be shown when multiple probesets are matched
    to the same gene.}
}
\details{
  The function can be useful when multiple matching of probesets are not
  of importance. If it was not the case, users should consider using
  \code{gtiChipAnnotation} to get the full annotation table of all
  probesets, and subset by GeneID/GeneSymbol to get \emph{all} probesets
  matched to the gene, rather than one arbitrary.
}
\value{
  A \code{data.frame} object similar as the one returned by \code{annotateProbesets}
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  See \code{\link{gtiChiptypes}} for supported chip types.

  See \code{\link{gtiChipAnnotation}} to get annotation for all
  probesets in a chip.

  See \code{\link{annotateProbesets}} to get annotation for probesets.
}
\examples{
options(error=utils::recover)

annotateGeneIDs(ids=c(780, 5982, 3310, NA), chip="HG-U133_PLUS_2")
annotateGeneIDs(ids=c(780, 5982, 3310, NULL), chip="HG-U133_PLUS_2")
annotateGeneSymbols(ids=c("DDR1", "RFC2", "HSPA6", "HSAP6"), chip="HUMAN_REFSEQ-8")

options(error=NULL)
}
