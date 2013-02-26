\name{matchGenes}
\alias{matchGenes}
\title{Get indices of genes in GeneSets}
\description{
  Given a vector of gene identifiers, match genes in GeneSets to get indices.
}
\usage{
matchGenes(x, vec, na.rm=TRUE)
}
\arguments{
  \item{x}{A GeneSets S3-object}
  \item{vec}{A vector of gene identifiers to be matched to}
  \item{na.rm}{Logical, whether \code{NA} should be removed in case no
    gene could be matched.}
}
\details{
  A common use of the function is to match GeneSet genes to gene
  identifiers of expression data. The vector is often the row names of
  the expression matrix or a column in the feature data.
}
\value{
  A list of integer indices. The list is named by gene set names.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
sample.gmt.file <- system.file("extdata", "test.gmt",package="ribiosIO")
test.gmt <- readGmt(sample.gmt.file)
test.vec <- c("AAK1", "ABL2", "ABI3BP", "ABCC10", "BAD")
matchGenes(test.gmt, test.vec)
}

