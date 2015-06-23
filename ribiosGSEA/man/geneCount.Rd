\name{geneCount}
\alias{geneCount}
\alias{geneCount.GeneSets}
\title{Get number of unique genes in GeneSets}
\description{
  Return number of unique genes in GeneSets
}
\usage{
geneCount(x, i)
}
\arguments{
  \item{x}{An \code{GeneSets} object}
  \item{i}{Subsetting index; if missing, counts of all gene sets
    are returned. }
}
\details{
  \code{NA}, \code{-} and empty string \dQuote{} do not count as valid
  gene identifiers. 
}
\value{
  A integers vector of gene counts, named after gene sets' names.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{geneCountFilter}}
}
\examples{
sample.gmt.file <- system.file("extdata", "test.gmt",package="ribiosIO")
test.gmt <- readGmt(sample.gmt.file)
geneCount(test.gmt)
geneCount(test.gmt, 2:3)
}
