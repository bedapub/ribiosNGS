\name{gsDescs}
\alias{gsDescs}
\title{GeneSets Descriptions}
\description{GeneSets descriptions}
\usage{
gsDescs(x, i)
}
\arguments{
  \item{x}{An GeneSets S3-object, probably returned from \code{readGmt}}
  \item{i}{Subsetting index; if missing, descriptions of all gene sets
    are returned.}
}
\value{
  Characters, description fields of gene sets
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{gsNames}},\code{\link{gsGenes}}
}
\examples{
sample.gmt.file <- system.file("extdata", "test.gmt",package="ribiosIO")
test.gmt <- readGmt(sample.gmt.file)
gsDescs(test.gmt)
gsDescs(test.gmt, c(1,2))
}
