\name{grp2gmt}
\alias{grp2gmt}
\alias{grpFiles2gmt}
\title{
  Convert GRP files into GMT-formatted strings
}
\description{
  GRP files are used by Connectivity Map on-line tool, which stores the
  information of a rank-ordered list of probesets. They are simply
  one-column text files, each line containing one
  probeset. \code{grpFiles2gmt} convert GRP files into GMT-formatted
  strings, which can be written in GMT files to be used by GSEA and
  other tools.

  The function \code{grp2gmt}, called by \code{grpFiles2gmt} internally,
  annotates probesets when \code{chiptype} is supported by GTI, and
  transform them into the GMT format.
}
\usage{
grp2gmt(txt, chiptype, name)
grpFiles2gmt(..., chiptype, n=-1L)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{txt}{A vector of character strings, each containing one probeset}
  \item{chiptype}{Chip type, normally character representing the
    microarray chip type. If the option is missing, or is of value \code{NA} or
    \code{NULL}, no annotation is done.}
  \item{name}{Character, name of the gene set (the first field of the
    GMT file)}
  \item{\dots}{GRP file names}
  \item{n}{Integer, number of lines to be read; \code{-1} indicates all
    lines should be read}
}
\details{
  If chiptype is provided, the \code{\link{annotate}} function is called
  to fetch probeset annotations from the databank. 
}
\value{
  A vector of character strings, each containing one line of a GMT
  file. They can be written to a file with the \code{\link{writeLines}} function.
}
\references{
  See \url{http://www.broadinstitute.org/cmap/index.jsp} for the use of
  GRP files in the Connectivity Map web tool.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\note{
  It is user's responsibility to check that all GRP files do exist and
  are readable.
}

\examples{
up.file <- system.file("extdata/tags_up.grp", package="ribiosExpression")
down.file <- system.file("extdata/tags_down.grp", package="ribiosExpression")
grp2gmt(readLines(up.file, n=-1))
grpFiles2gmt(c(up.file, down.file), n=3)

\dontrun{
grp2gmt(readLines(up.file, n=-1), chiptype="HG_U95AV2")
grpFiles2gmt(c(up.file, down.file), n=-1L, chiptype="HG_U95AV2")
}
}
