\name{geneCountFilter}
\alias{geneCountFilter}
\title{Filter GeneSets by gene counts}
\description{
  Filter GeneSets by gene counts
}
\usage{
geneCountFilter(x,min,max)
}
\arguments{
  \item{x}{An GeneSets S3-object}
  \item{min}{minimal number of genes}
  \item{max}{maximal number of genes}
}
\details{
  Gene sets with fewer genes than \code{min} or more genes than
  \code{max} will be removed.   Duplicated genes in genesets are only counted once.
}
\value{
  A \code{GeneSets} object
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
test.gmt <- lapply(1:10, function(x) list(name=paste("GeneSet", x),
description="", genes=1:x))
class(test.gmt) <- "GeneSets"

length(test.gmt)
length(geneCountFilter(test.gmt, min=5))
length(geneCountFilter(test.gmt, min=5, max=8))
}

