% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mps-io.R
\name{readMpsnakeAsDGEList}
\alias{readMpsnakeAsDGEList}
\title{Read mpsnake output directory into a DGEList object}
\usage{
readMpsnakeAsDGEList(dir, minReads = NULL)
}
\arguments{
\item{dir}{Character string, path of mpsnake pipeline directory (or the \code{results} subdirectory).}

\item{minReads}{\code{NULL} or tnteger, minimalistic read numbers for a sample to be considered. In case of \code{NULL}, no filtering is performed.}
}
\value{
A \code{DGEList} object containing counts, gene, and sample annotation
}
\description{
Read mpsnake output directory into a DGEList object
}
\examples{
mpsnakeDir <- system.file("extdata/mpsnake-minimal-outdir", package="ribiosNGS")
mpsDgeList <- readMpsnakeAsDGEList(mpsnakeDir)

## equivalent
mpsnakeResDir <- system.file("extdata/mpsnake-minimal-outdir", "results",
  package="ribiosNGS")
mpsDgeList <- readMpsnakeAsDGEList(mpsnakeResDir)
}
