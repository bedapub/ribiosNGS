\name{gtiChipAnnotation}
\alias{ORACLE.IN.NMAX}
\alias{gtiChipAnnotation}
\alias{annotateChip}
\alias{raceChipAnnotation}
\alias{biosCurrentGeneSymbol}
\title{
  Annotations of all features associated with the given chip type
}
\description{
  The function returns annotations (see details below) of all or selected features
  (probably probesets) associated with the given chip type. 
}
\usage{
gtiChipAnnotation(chip, ids, orthologue=FALSE, multiOrth=FALSE)
annotateChip(chip, ids, orthologue=FALSE, multiOrth=FALSE)
}
\arguments{
  \item{chip}{Character string, the chip type in interest. For a
    complete list of supported chip names, use the
    \code{gtiChiptypes()} function.}
  \item{ids}{Optional, character string vectors. IDs (probably
    probesets) to be queried in the given chip}
  \item{orthologue}{Logical, should genes be mapped to human
    orthologues? Default is \code{FALSE}}
  \item{multiOrth}{Logical, should multiple-mapping orthologues all
    returned. Default is \code{FALSE}}
}
\details{
  The function is useful when someone wants to annotate all probesets on a
  certain chip type, or some probesets of a known chip type. It connects
  to the GTI Oracle database and performs the annotation.

  It is also able to perform orthologue mapping by setting
  \code{orthologue} to \code{TRUE}.

  \code{annotateChip} is the synonymous function of \code{gtiChiptype}.
}
\value{
 A \code{data.frame} object.

 If \code{orthologue=FALSE}, the data.frame contains following columns
 \item{ProbeID}{Probeset ID}
 \item{GeneID}{Entrez Gene ID}
 \item{GeneSymbol}{Entrez (HUGO) Gene Symbol}
 \item{GeneName}{Gene descriptions}
 \item{Chip}{Chip type}
 \item{TaxID}{NCBI Taxonomy ID}
 
 If \code{orthologue=TRUE}, the data.frame contains following columns:
  \item{ProbeID}{Probeset ID}
  \item{GeneID}{Entrez Gene ID of human orthologue gene (if existing)}
  \item{GeneSymbol}{Entrez (HUGO) Gene Symbol of human orthologue gene
    (if existing)}
  \item{GeneName}{Gene descriptions of original gene}
  \item{Chip}{Chip type}
  \item{TaxID}{Human taxonomy ID (9606)}
  \item{OrigGeneID}{Entrez Gene ID of the gene to which queried probesets map}
  \item{OrigGeneSymbol}{Gene Symbol of the gene to which queried probesets map}
  \item{OrigTaxID}{Taxonomy ID of the gene to which queried probesetsmap}
}
\references{
\url{http://kephar.bas.roche.com/bioinfo/apps/chip/Chip_probes_GTI_interface.htm}
}
\author{Jitao David Zhang <jitao_david.zhang@roche.com>}
\section{Warning}{
  If \code{multiOrth} is \code{TRUE}, the returning data.frame may
  contain more rows than the number of input probesets.This is not
  desired in situations where the output contains exactly the same
  number of probesets as the input, e.g. annotating an
  \code{ExpressionSet} object.
}
\seealso{
  \code{\link{gtiChiptypes}} for a complete list of supported chip
  types.
}
\note{
  From version 2.0-0 and on, \code{isSingleGeneID} has been removed.
  
  From version 1.0-15 and on, \code{isSingleGeneID} is always set to
  \code{TRUE} since only uniquely mapped probesets are
  stored. Provisionally this column shall be removed in the future
  version.
}
\examples{
\dontrun{
mychip <- "HG-U133_PLUS_2"
mychipAnno <- gtiChipAnnotation(mychip)
dim(mychipAnno)
head(mychipAnno)
}

options(error=utils::recover)
gtiChipAnnotation(mychip, ids=c("1053_at","117_at", "121_at"))
gtiChipAnnotation(mychip, ids=c("1053_at","117_at", "121_at"),
orthologue=TRUE)
gtiChipAnnotation(mychip, ids=c("1053_at","117_at", "121_at"),
orthologue=TRUE, multiOrth=TRUE)
options(error=NULL)
}
