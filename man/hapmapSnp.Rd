\name{hapmapSnp}
\alias{hapmapSnp}
\alias{illuminaSnp}
\title{
  Get HAPMAP SNP annotation and chromosomal location
}
\description{
  \code{hapmapSnp} queries database with a vector of input HAPMAP SNP
  identifiers, in the form of \code{rsXXXXX}, where \code{XXXXX} are
  integer numbers, and returns a data.frame containing 
}
\usage{
hapmapSnp(ids, genes = FALSE, flanking = FALSE)
illuminaSnp(ids, genes = FALSE)
}
\arguments{
  \item{ids}{A character vector, containing SNP identifiers}
  \item{genes}{Logical, whether the information of genes residing on or
    around the SNP should be returned}
  \item{flanking}{Logical, whether the flanking DNA sequences should be
    returned}
}
\details{
  The \code{hapmapSnp} function returns SNP annotations from the HapMap
  project. When \code{genes} is set to \code{TRUE}, the function returns
  the gene in which SNP resides if it exists, or the closest genes and
  the distances from SNP to them. A side effect of using this
  information is that the output \code{data.frame} usually does not have
  the same row number as the length of the input vector, since it is
  quite common that a SNP is annotated with two nearest genes (up- and
  down-stream). User may want to check their results for these cases manually.

  The \code{illuminaSnp} function probably reads the SNP table prodived by the Illumina company
  (this assition shall be checked for truth). It should be used
  cautiously since we are not quite sure about the data annotation procedure.
}
\value{
  A \code{data.frame} object containing at least following columns:
  \item{SNP_ID}{SNP ID in the form of rsXXXXX}
  \item{Chromosome}{On which chromosome (currently, Feb 2012, we are
    using the hg19 coordinates)}
  \item{Position}{Chromosome coordinate of the SNP}
  \item{Allele1}{Most frequent Allele}
  \item{Allele2}{Secondly frequent Allele}

  An extra column named \code{FlankingSeq} is available when
  \code{flanking} is set to \code{TRUE}, providing flanking sequences.
  
  When \code{genes=TRUE}, following columns are available
  \item{ClosestGeneID}{Entrez GeneID of the closest gene. Note that when
    DistanceToGene is 0, this is the GeneID of the gene where the
    SNP resides} 
  \item{DistanceToGene}{Distance to the closest gene (in bp). SNP in a
    gene is annotated as 0.}
  \item{GeneStart}{Where does the gene start}
  \item{GeneStop}{Where does the gene stops}
}
\references{
\url{http://bioinfo.bas.roche.com:8080/apps/hapmap/}
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
options(error=utils::recover)

snpIds <- c("rs884080", "rs557477", "rs531099","rs763318")
hapmapSnp(snpIds)
hapmapSnp(snpIds, flanking=TRUE, genes=TRUE)
hapmapSnp(snpIds, flanking=TRUE, genes=FALSE)
hapmapSnp(snpIds, flanking=FALSE, genes=TRUE)

illuminaSnp(snpIds)
illuminaSnp(snpIds, genes=TRUE)
options(error=NULL)
}

