\name{gsDesc}
\alias{gsDesc}
\title{GeneSets Descriptions}
\description{GeneSets description}
\usage{
gsDesc(x, i)
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
  \code{\link{gsName}},\code{\link{gsGenes}}
}
\examples{
sample.gmt.file <- system.file("extdata", "test.gmt",package="ribiosIO")
test.gmt <- readGmt(sample.gmt.file)
gsDesc(test.gmt)
gsDesc(test.gmt, c(1,2))
}
