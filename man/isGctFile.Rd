\name{isGctFile}
\alias{isGctFile}
\title{
  Test a file is a GCT file or not.
}
\description{
  The function tests whether a file complies with the GCT file format.
}
\usage{
isGctFile(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{Character string, a file name.}
}
\details{
  A file is a valid GCT file if it meets following three rules:
  \enumerate{
  \item The first line of the file is \dQuote{#1.2}
  \item The second line contains number of rows and number of
    columns, separated by a tab.
  \item The rest of file contain a rectangular matrix, with the first
    two columns named \dQuote{NAME} and \dQuote{Description}
    respectively.
  }
  This function relaxes some of the specifications. In rule 2, the tab
  can be replaced by any empty symbol grepped by \sQuote{\\s}. In rule
  3, the column names are case in-sensitive.
}
\value{
  A logical value: \code{TRUE} means \code{file} is of the GCT format.
}
\references{
  \code{http://www.broadinstitute.org/cancer/software/genepattern/tutorial/gp_fileformats}
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{read_gct_matrix} to read in GCT files.
}
\examples{
myInFile <- system.file("extdata/test.gct", package="ribiosIO")
isGctFile(myInFile)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
