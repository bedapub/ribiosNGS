\name{filterGmtByGeneCount}
\alias{filterGmtByGeneCount}
\title{
  Filter a GMT list by gene counts
}
\description{
  Removing very small or very large gene sets.
}
\usage{
filterGmtByGeneCount(gmtlist, min = 5L, max = 1000L)
}
\arguments{
  \item{gmtlist}{A GMT list, each item is a list of three components:
    \code{name}, \code{description} and \code{genes}.}
  \item{min}{Minimum number of genes in a gene set}
  \item{max}{Maximum number of genes in a gene set}
}
\note{
  Duplicated genes in genesets are only counted once.
}
\value{
  A GMT list with filtered gene sets.
}
\examples{
if(require(ribiosIO)) {
  test.gmt <- read_gmt_list(system.file("extdata", "test.gmt",
package="ribiosIO"))
  length(test.gmt)
  test.gmt.filter <- filterGmtByGeneCount(test.gmt, min=1022, max=1032)
  length(test.gmt.filter)
}
}

