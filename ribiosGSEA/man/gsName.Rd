\name{gsNames}
\alias{gsNames}
\title{GeneSet names}
\description{Gene set names}
\usage{
gsNames(object, i)
}
\arguments{
  \item{object}{An GeneSets S3-object, probably returned from \code{readGmt}}
  \item{i}{Subsetting index; if missing, names of all gene sets
    are returned. }
}
\value{
    Characters, name fields of gene sets.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{gsDescs}},\code{\link{gsGenes}}
}
\examples{
sample.gmt.file <- system.file("extdata", "test.gmt",package="ribiosIO")
test.gmt <- readGmt(sample.gmt.file)
gsNames(test.gmt)
gsNames(test.gmt, c(1,2))
}

