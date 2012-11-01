\name{write_gmt}
\alias{write_gmt}
\title{
  Write a list of gene sets into a GMT file
}
\description{
  Write gene-sets in a GMT-list form into GMT files.
}
\usage{
write_gmt(gmt, file)
}
\arguments{
  \item{gmt}{A list of gene sets, each item is a list of three
    components, named \sQuote{name}, \sQuote{description} and
    \sQuote{genes}. }
  \item{file}{The GMT file to create}
}
\details{
  This function can be used, for instance, to combine multiple GMT files
  into a new one.
}
\value{
  Invisible NULL when the file is successfully created. Otherwise an
  error message will be printed.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
idir <- system.file("extdata", package="ribiosIO")
sample.gmt.file <- file.path(idir, "test.gmt")

test.gmt <- read_gmt_list(sample.gmt.file)

outgmt.file <- paste(tempfile(), ".gmt", sep="")

write_gmt(test.gmt[1:2], file=outgmt.file)
}