\name{gsGenes}
\alias{gsGenes}
\title{Get genes stored in GeneSet objects}
\description{
  Get genes stored in GeneSet objects
}
\usage{
gsGenes(object, i)
}
\arguments{
  \item{object}{An GeneSets S3-object, probably returned from \code{readGmt}}
  \item{i}{Subsetting index; if missing, descriptions of all gene sets
    are returned.}
}
\value{
  If \code{i} is missing or has length larger than 1, the function
  returns a list of genes.

  If \code{i} has the length of 1, the function returns a vector of
  genes, of the gene set indexed by \code{i}.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{gsDescs}},  \code{\link{gsNames}},
}
\examples{
sample.gmt.file <- system.file("extdata", "test.gmt",package="ribiosIO")
test.gmt <- readGmt(sample.gmt.file)

allGenes <- gsGenes(test.gmt)
str(allGenes)

subGenes <- gsGenes(test.gmt, c(1,2))
str(subGenes)

str(gsGenes(test.gmt), 1L)
}

