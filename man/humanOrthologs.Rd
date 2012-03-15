\name{humanOrthologs}
\alias{humanOrthologs}
\alias{humanUniqOrtholog}
\alias{nonhumanOrthologs}
\alias{nonhumanUniqOrtholog}
\title{
  Mapping between orthologous genes in human and in other taxa
}
\description{
  \code{humanOrthologs} and \code{nonhumanOrthologs} are two basic
  functions to map between orthologous genes in human and in other taxa,
  mainly in rat and/or in mouse, using information stored in the GTI
  database. \code{humanOrthologs} get all orthologous genes in human of a vector of
  input genes in other taxon. \code{nonhumanOrthologs} makes the
  revserve mapping, namely getting orthologs of human genes in other taxon.

  \code{humanUniqOrtholog} and \code{nonhumanUniqOrtholog} are wrappers
  to return exact one ortholog for each gene. No complex logic is
  implemented trying to guess which ortholog is the best. Users in want
  of such behaviour should start from \code{humanOrthologs} and
  \code{nonhumanOrthologs}. See details below.
}
\usage{
humanOrthologs(geneids)
nonhumanOrthologs(geneids, taxid)
humanUniqOrtholog(geneids)
nonhumanUniqOrtholog(geneids, taxid)
}
\arguments{
  \item{geneids}{A vector of integers or charaters, giving Entrez
    GeneIDs.}
  \item{taxid}{NCBI Taxon ID, vector of integer or character. For \code{nonhumanOrthologs}, if set to
    \code{NULL} (default), all orthologs available will be
    returned. If a vector is given, only orthologs in these specified
    taxa are returned. For \code{nonhumanUniqOrtholog}, onle one Taxon
    ID is accepted.}
}
\details{
  For a vector of GeneID in query, \code{humanOrthologs} and
  \code{nonhumanOrthologs} returns a list in the same order as the input
  vector, each list item containing a vector of Entrez GeneIDs that are
  ortholog(s) of the input gene, or \code{NULL} if no ortholog was found.
  
  \code{humanUniqOrtholog} and \code{nonhumanUniqOrtholog} are wrappers
  that ensure each gene has has one ortholog returned. This is done by
  only keeping the gene with the minimum GeneID. This is only a
  pragmatic solution, since for genes with multiple Orthologs this
  causes loss of information. However, we note that only a few genes
  have more than one orthologs (for example <5\% in mouse), and we
  hypothesize using any one of them will not affect high-throughput
  analysis, for example gene set enrichment analysis, much. Choosing the
  minimum GeneID garantees the function runs determinsitically, that
  means it returns the same result every time so far the underlying
  database remains unchanged.

}
\value{
  \code{humanOrthologs} and \code{nonhumanOrthologs} return a list in
  the same length and exactly the same order as the input vector of
  GeneIDs. Each list item contains a vector of ortholog GeneIDs, or
  \code{NULL}. For \code{nonhumanOrthologs}, if genes from more than one
  taxon is returned, Taxon IDs are used as name of list items.

  \code{humanUniqOrtholog} and \code{nonhumanUniqOrtholog} return a
  vector in the same length and exactly the same order as the input
  vector.It contains Entrez GeneIDs of orthologous genes of queried
  GeneIDs. For genes without available orthologs, \code{NA}s are returned.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com> and Laura Badi <laura.badi@roche.com>
}
\examples{
options(error=utils::recover)
humanOrthologs(c(100017, 100019, 100012, 100037258))
humanUniqOrtholog(c(100017, 100019, 100012, 100037258))

nonhumanOrthologs(c(1432, 5611, 26119))
nonhumanOrthologs(c(1432, 5611, 26119), taxid=10116)
nonhumanOrthologs(c(1432, 5611, 26119), taxid=c(10116,10090))
nonhumanUniqOrtholog(c(1432,5611,26119), taxid=10116)
options(error=NULL)
}
