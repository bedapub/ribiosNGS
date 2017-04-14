\name{read_biokit_exprs}
\alias{read_biokit_exprs}
\title{
  Read expression files output by the Biokit pipeline 
}
\description{
  The function reads expression files output by the Biokit pipeline. A brief description can be found in the \code{description} section.
}
\usage{
read_biokit_exprs(filename)
}
\arguments{
  \item{filename}{A Roche NGS pipeline output file}
}
\details{
  An Roche NGS expression file is eseentially a tab-delimited file. THe
  first six columns are mandatory (feature/tag name, multiple mapping
  RPKM, multiple mapping read count, unique mapping RPKM, unique mapping
  read count, and multiple mapping proportion). Right to these columns
  there can be arbitrary numbers of columns appended to annotate the
  features.

  In the current output, rows may have different numbers of columns:
  particularly for features without corresponding items in the
  annotation file used in the pipeline, their rows will contain the
  mandatory columns plus one extra column with the value
  \dQuote{unknown}. This is handled automatically by the function.
}
\value{
  A \code{data.frame} contains both mandatory and additional
  columns. The first column of the expression file will be used as the
  row names of the \code{data.frame} object.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{read_gct} for reading gct files, a commonly used file format for
  expression data.
}
\examples{
ngsFile <- system.file("extdata/NGS_exprs/file1.expression", package="ribiosIO")
ngs <- read_biokit_exprs(ngsFile)
}
