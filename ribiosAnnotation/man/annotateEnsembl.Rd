\name{annotateEnsembl}
\alias{annotateEnsembl}
\title{
  Annotate Ensembl gene/transcript/protein identifiers
}
\description{
  Annotate Ensembl identifiers, optionally with human orthologue mappings
}
\usage{
annotateEnsembl(ids, orthologue=FALSE, multiOrth=FALSE)
}
\arguments{
  \item{ids}{Character vector, Ensembl gene (ENSG)/transcript
    (ENST)/protein (ENSG) identifiers . It can contain \code{NA} or \code{NULL}}
  \item{orthologue}{Logical, whether human orthologues should be returnd}
  \item{multiOrth}{Logical, whether multiple mapped orthologues should
    be returned or not. Only useful when \code{orthologue} is
    \code{TRUE}, By default \code{FALSE}. Be cautious with the
    \code{TRUE} option: in this case the returning \code{data.frame}
    may have different row numbers as the input vector.}
}
\value{
  A \code{data.frame} object containing the annotations.

  If \code{orthologue} is set to \code{FALSE}, the data frame contains
  following columns: \code{ENSEMBL_ID, GeneID, GeneSymbol, GeneName and TaxID} 

  If \code{orthologue} is \code{TRUE}, the data frame contains
  \code{ENSEMBL_ID, GeneID, GeneSymbol,TaxID, OrigTaxID, OrigGeneID, OrigGeneSymbol,
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

  See \code{\link{annotateGeneIDs}} to get annotation for Entrez GeneIDs.
  
  See \code{\link{annotateProbesets}} to get annotation for probesets.
}
\examples{
options(error=utils::recover)

## normal use
annotateEnsembl(ids=c("ENST00000456328", "ENST00000541675", NA))

annotateEnsembl(ids=c("ENSG00000112062", "ENST00000229795", "ENSP00000229795", NULL))

## TODO: fix the error of the following query
## annotateEnsembl(ids="ENSMUSG00000031710", orthologue=TRUE)

options(error=NULL)
}
