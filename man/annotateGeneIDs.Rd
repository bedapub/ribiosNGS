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
annotateGeneIDs(ids, orthologue=FALSE)
annotateGeneSymbols(ids, orthologue=FALSE, organism=c("human", "mouse", "rat", "any"))
}
\arguments{
  \item{ids}{Character vector, GeneIDs or GeneSymbols of query. It can contain \code{NA} or \code{NULL}}
  \item{orthologue}{Logical, whether human orthologues should be
    returnd}
  \item{organism}{Character string, specifies the organism from which
    input gene symbols com from. This option is only valid for
    GeneSymbol since it is commonly not unique.In case of \sQuote{any},
    all genes with the matching GeneSymbol, irrespective of the species,
    will be returned. In this special case the number of rows of the
    resulting \code{data.frame} can be different from the number of
    input GeneSymbols.}
}
\details{
  Prior to version \code{1.2-0}, GeneIDs and GeneSymbols are mapped via
  chip annotation files, which introduce uncessary dependency and
  confusing results when chip does not contain all genes.

  From version \code{1.2-0}, \code{annotateGeneIDs} and
  \code{annotateGeneSymbols} do not dependent on chip annotations, but
  on the core tables of GTI, which can be more efficient. In addition,
  new functionalities to map orthologues are added.
  
}
\value{
  A \code{data.frame} object similar as the one returned by \code{annotateProbesets}
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  See \code{\link{gtiChipAnnotation}} to get annotation for all
  probesets in a chip.

  See \code{\link{annotateProbesets}} to get annotation for probesets.
}
\examples{
options(error=utils::recover)

## normal use
annotateGeneIDs(ids=c(780, 5982, 3310, NA))
annotateGeneIDs(ids=c(780, 5982, 3310, NULL))
annotateGeneSymbols(ids=c("DDR1", "RFC2", "HSPA6",
"HSAP6"),organism="human", orthologue=FALSE)

## note that GeneSymbols are not unique
annotateGeneSymbols(ids=c("DDR1", "RFC2", "HSPA6", "HSAP6"),organism="any", orthologue=FALSE)

## orthologues
annotateGeneIDs(ids=c(1017, 12566, 362817, 26416, 81649),
orthologue=FALSE)
annotateGeneIDs(ids=c(1017, 12566, 362817, 26416, 81649),
orthologue=TRUE)
annotateGeneSymbols(ids=c("DDR1", "RFC2", "HSPA6",
"HSAP6"),organism="any", orthologue=TRUE)

## Following examples underlines the non-uniqueness of GeneSymbols
annotateGeneSymbols(ids=c("Cdk2", "CDK4", "Mapk14"),organism="rat",
orthologue=TRUE)
annotateGeneSymbols(ids=c("Cdk2", "CDK4", "Mapk14"),organism="mouse", orthologue=TRUE)
annotateGeneSymbols(ids=c("Cdk2", "CDK4", "Mapk14"),organism="any", orthologue=TRUE)

options(error=NULL)
}
