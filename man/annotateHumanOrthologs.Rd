\name{annotateHumanOrthologs}
\alias{annotateHumanOrthologs}
\title{Annotate human orthologues of genes with given GeneID}
\description{
  The function maps any given GeneID to their human orthologues, and
  annotate them with essential information.
}
\usage{
annotateHumanOrthologs(geneids, multiOrth = FALSE)
}
\arguments{
  \item{geneids}{Character or numeric vector, Entrez GeneIDs to be queried}
  \item{multiOrth}{Logical, whether multiple orthologues to the same
    gene should be all returned. By default \code{FALSE}}
}
\details{
  The function maps any GeneID to human orthologues by querying Oracle
  database.
}
\value{
  A \code{data.frame} containing following columns:
  \item{OrigGeneID}{Entrez Gene ID in query}
  \item{OrigTaxID}{Taxonomy ID of input Entrez Gene ID}
  \item{TaxID}{Taxonomy ID of orthologue Entrez Gene ID. Currently it is
  fixed as 9606, the human TaxID.}
  \item{GeneID}{Entrez Gene ID of human orthologue gene}
  \item{GeneSymbol}{GeneSymbol of human orthologue gene}
}
\references{
  \url{http://kephar.bas.roche.com/bioinfo/apps/gti/HOW_design_and_implementation/GTI\%20core\%20tables_files/GTI\%20core\%20tables_frames.htm}
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{humanOrthologs}} and \code{\link{humanUniqOrtholog}}
}
\examples{
options(error=utils::recover)
annotateHumanOrthologs(c(100034253, 100036582, 1000125361, 1432))
options(error=NULL)
}
