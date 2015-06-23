\name{annotateAnyProbeset}
\alias{annotateAnyProbeset}
\title{Annotate any probeset without knowing chiptype}
\description{
  Annotate a set of probesets possibily of mixed chip types
}
\usage{
annotateAnyProbeset(ids, orthologue = FALSE, multiOrth = FALSE)
}
\arguments{
  \item{ids}{Character vector, probesets to be annotated}
  \item{orthologue}{Logical, should genes be mapped to human orthologues.}
  \item{multiOrth}{Logical, should multiple orthologues all be returned.}
}
\details{
  The function annotates input probesets with essential
  information. These probesets probably come from unknown and mixed
  chiptypes. In case they come from a known, single chip type, consider
  using \code{\link{gtiChipAnnotation}} which is faster; in case they
  are known to come from a single chip but the type is unknown, consider
  using \code{\link{guessChiptype}} first to guess the most likely chip
  type and then use \code{\link{gtiChipAnnotation}}.
}
\value{
  If \code{orthologue=FALSE}, the data.frame contains following columns
  \item{ProbeID}{Probeset ID}
  \item{GeneID}{Entrez Gene ID}
  \item{GeneSymbol}{Entrez (HUGO) Gene Symbol}
  \item{GeneName}{Gene descriptions}
  \item{Chip}{Has NA as value}
  \item{TaxID}{NCBI Taxonomy ID}
  
  If \code{orthologue=TRUE}, the data.frame contains following columns:
  \item{ProbeID}{Probeset ID}
  \item{GeneID}{Entrez Gene ID of human orthologue gene (if existing)}
  \item{GeneSymbol}{Entrez (HUGO) Gene Symbol of human orthologue gene
    (if existing)}
  \item{GeneName}{Gene descriptions of original gene}
  \item{Chip}{Has NA as value}
  \item{TaxID}{Human taxonomy ID (9606)}
  \item{OrigGeneID}{Entrez Gene ID of the gene to which queried probesets map}
  \item{OrigGeneSymbol}{Gene Symbol of the gene to which queried probesets map}
  \item{OrigTaxID}{Taxonomy ID of the gene to which queried probesetsmap}
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\section{Warning}{
  If \code{multiOrth} is \code{TRUE}, the returning data.frame may
  contain more rows than the number of input probesets.This is not
  desired in situations where the output contains exactly the same
  number of probesets as the input, e.g. annotating an
  \code{ExpressionSet} object.
}
\seealso{
  \code{\link{gtiChipAnnotation}} and \code{\link{guessChiptype}}
}
\examples{
options(error=utils::recover)
myProbesets <- gtiChipAnnotation(chip="HG-U133_PLUS_2")
freeProbesets <- annotateAnyProbeset(ids=myProbesets$ProbeID[1:100])
system.time(freeProbesetsSomeOrtho <- annotateAnyProbeset(ids=myProbesets$ProbeID[1:100],
                                                          orthologue=TRUE))
options(error=NULL)
}
