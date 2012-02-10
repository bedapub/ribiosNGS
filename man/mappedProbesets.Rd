\name{mappedProbesets}
\alias{mappedProbesets}
\title{
  Find all mapped probesets to given Entrez GeneIDs in a chip
}
\description{
  The function fetched all probesets that map to Entrez GeneIDs of user
  input in a specified chip type.

  See Details for the different from the
  \code{\link{annotateGeneIDs}}. Briefly, if the aim is to find all
  probesets that are mapped to Entrez GeneIDs, not caring about other
  annotation information like the Gene Symbols, one should use
  \code{mappedProbesets}. Otherwise, if one is interested whether a
  GeneID is mapped to \emph{any} probeset in a chip, and its annotation
  (GeneSymbol, GeneName, etc), one should use \code{mappedProbesets}.

}
\usage{
mappedProbesets(geneids, chip = "HG-U133_PLUS_2", isSingleGeneID = FALSE, unlist = FALSE)
}
\arguments{
  \item{geneids}{A vector of Entrez GeneIDs, given in integers or characters
    representing integers of GeneIDs.}
  \item{chip}{Mandatory, a character string, chiptype.}
  \item{isSingleGeneID}{Logical. If set to \code{TRUE}, only probesets
    mapped to unique GeneIDs will be returned.}
  \item{unlist}{Logical. If set to \code{TRUE}, the result will be a
    character vector; otherwise, it will be a list indexed by Entrez
    GeneIDs for which at least one probesets were found.}
}
\details{
  The difference from \code{\link{annotateGeneIDs}} is that
  \code{mappedProbesets} returns all probesets that are mapped, while
  \code{\link{annotateGeneIDs}} returns one arbitrary probeset and all
  other relevant information including GeneSymbol, GeneName, etc. The
  difference is rooted in the aims of the two functions:
  \code{mappedProbesets} are used to find all probesets that may
  potentially represent given genes, while \code{annotateGeneIDs} use a
  chip-annotation table rather like a bridge to get annotation
  information for GeneIDs.
}
\value{
  A list indexed by Entrez GeneIDs, for which at least one probesets
  were found (\code{unlist=TRUE}), or a character vector of probesets (\code{unlist=FALSE}).
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  See \code{\link{gtiChipAnnotation}} for retrieving annotation information for
  a chip, and \code{\link{annotateGeneIDs}} for retrieving arbitrary one
  probeset for Entrez GeneIDs in query.
}
\examples{
options(error=utils::recover)
myGeneIDs <- c(1,1234,1432, 245908)
mappedProbesets(myGeneIDs, chip="HG-U133_PLUS_2")
mappedProbesets(myGeneIDs, chip="HG-U133_PLUS_2", isSingleGeneID=TRUE)
mappedProbesets(myGeneIDs, chip="HG-U133_PLUS_2", isSingleGeneID=TRUE,
unlist=TRUE)
options(error=NULL)
}
