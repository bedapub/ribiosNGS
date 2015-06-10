\name{read_chip}
\alias{read_chip}
\title{
  Read CHIP file
}
\description{
  The CHIP file format is commonly used to annotate probesets or other
  identifiers to gene symbols and gene names.  This function imports
  CHIP files, using a C procedure to accelerate the speed.
}
\usage{
read_chip(x)
}
\arguments{
  \item{x}{File name}
}
\details{
  A data.frame is returned with three columns: \code{ProbeID},
  \code{GeneSymbol} and \code{GeneName}.
}
\value{
  A data.frame is returned with three columns: \code{ProbeID},
  \code{GeneSymbol} and \code{GeneName}.
}
\references{
  BROAD institute GSEA manual
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
  testFile <- system.file("extdata/test.chip", package="ribiosIO")
  testChip <- read_chip(testFile)
  head(testChip)
}
