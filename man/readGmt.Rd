\name{readGmt}
\alias{readGmt}
\title{Read GMT file into a GeneSets object}
\description{
  Read GMT file into a GeneSets S3-object
}
\usage{
readGmt(file)
}
\arguments{
  \item{file}{A GMT file}
}
\details{
  The function is a wrapper of the \code{read_gmt_list} function in
  \code{ribiosIO}, which provides C-level speed of reading GMT files
  into a list object in R. \code{readGmt} wraps the list into a S3 class
  (\code{GeneSets}) in order to simplify subsequent manipulations of
  gene sets.
}
\value{A \code{GeneSets} object}
\seealso{
  \code{\link{read_gmt_list}}
}
\examples{
sample.gmt.file <- system.file("extdata", "test.gmt",package="ribiosIO")
test.gmt <- readGmt(sample.gmt.file)
test.gmt
}

