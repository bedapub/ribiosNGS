\name{annotateGeneIDs}
\alias{annotateGeneIDs}
\alias{annotateGeneSymbols}
\title{
  Annotate GeneIDs or GeneSymbols
}
\description{
  Annotate GeneIDs or GeneSymbols, optionally with human orthologue mappings
}
\usage{
annotateGeneIDs(ids, orthologue=FALSE, multiOrth=FALSE)
annotateGeneSymbols(ids, organism=c("human", "mouse", "rat", "any"),orthologue=FALSE, multiOrth=FALSE)
}
\arguments{
  \item{ids}{Character vector, GeneIDs or GeneSymbols of query. It can contain \code{NA} or \code{NULL}}
  \item{organism}{Character string, specifies the organism from which
    input gene symbols com from. This option is only valid for
    GeneSymbol since it is commonly not unique.In case of \sQuote{any},
    all genes with the matching GeneSymbol, irrespective of the species,
    will be returned. In this special case the number of rows of the
    resulting \code{data.frame} can be different from the number of
    input GeneSymbols.}
  \item{orthologue}{Logical, whether human orthologues should be returnd}
  \item{multiOrth}{Logical, whether multiple mapped orthologues should
    be returned or not. Only useful when \code{orthologue} is
    \code{TRUE}, By default \code{FALSE}. Be cautious with the
    \code{TRUE} option: in this case the returning \code{data.frame}
    may have different row numbers as the input vector.}
}
\details{
  Prior to version \code{1.2-0}, GeneIDs and GeneSymbols are mapped via
  chip annotation files, which introduce uncessary dependency and
  confusing results when chip does not contain all genes.

  From version \code{1.2-0}, \code{annotateGeneIDs} and
  \code{annotateGeneSymbols} do not dependent on chip annotations, but
  on the core tables of GTI, which can be more efficient. In addition,
  new functionalities to map orthologues are added.

  From version \code{2.0-0}, the speed of both functions are further optimized.
}
\value{
  A \code{data.frame} object containing the annotations.

  If \code{orthologue} is set to \code{FALSE}, the data frame contains
  following columns: \code{GeneID, GeneSymbol, GeneName and TaxID} 

  If \code{orthologue} is \code{TRUE}, the data frame contains
  \code{GeneID, GeneSymbol,TaxID, OrigTaxID, OrigGeneID, OrigGeneSymbol,
    OrigGeneName}. Note that \code{GeneID, GeneSymbol, TaxID} contains
  the information of mapped orthologues, while \code{OrigXXX} contains
  the information of queried genes.

  If \code{multiOrth} is \code{TRUE} (only valid when \code{orthologue}
  is \code{TRUE}), multiples orthologues mapped to the same gene are all
  returned. This means that the result data frame may contain more rows
  than the length of input vector. If set to \code{FALSE}, the resulting
  data frame contains exact number of rows as the input vector length.
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
annotateGeneIDs(ids=c(1017, 12566, 362817, 26416, 81649),
orthologue=TRUE, multiOrth=TRUE)

## Following examples underlines the non-uniqueness of GeneSymbols
annotateGeneSymbols(ids=c("Cdk2", "CDK4", "Mapk14"),organism="rat",
orthologue=TRUE)
annotateGeneSymbols(ids=c("Cdk2", "CDK4", "Mapk14"),organism="mouse", orthologue=TRUE)
annotateGeneSymbols(ids=c("Cdk2", "CDK4", "Mapk14"),organism="any", orthologue=TRUE)

options(error=NULL)
}
