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
  The current implementation only parses the first three columns and ignores the rest of columns. This behavior may change in future versions to provide larger flexibility of parsing CHIP-like files.
}
\value{
  A data.frame is returned with three columns: \code{ProbeSetID},
  \code{GeneSymbol} and \code{GeneTitle}. The column names are concordant with the GSEA convention, except that the empty spaces are omitted.
}
\references{
  BROAD institute GSEA manual, available at \url{http://software.broadinstitute.org/cancer/software/gsea/wiki/index.php/Data_formats#CHIP:_Chip_file_format_.28.2A.chip.29}.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
  testFile <- system.file("extdata/test.chip", package="ribiosIO")
  testChip <- read_chip(testFile)
  head(testChip)
  stopifnot(identical(colnames(testChip), c("ProbeSetID", "GeneSymbol", "GeneTitle")))
}
