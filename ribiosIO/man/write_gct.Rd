\name{write_gct}
\alias{write_gct}
\title{
  Write matrix in GCT file format
}
\description{
  Write matrix in GCT file format
}
\usage{
write_gct(matrix, file = stdout(), feat.name, feat.desc, na="")
}
\arguments{
  \item{matrix}{A numeric matrix}
  \item{file}{Output file name. By default the file is written to
    standard output}
  \item{feat.name}{Character vector, optional. Feature names; if missing
    the row names are used as feature names. If given, \code{feat.name} must be
    of the same length as the row number of the input matrix.}
  \item{feat.desc}{Character vector, optional. Feature descriptions; if
    missing, empty strings will be used as descriptions.}
  \item{na}{Character string, how 'NA' values will be printed?}
}
\details{
  Input matrix will be transformed into the GCT format. The transformed
  texts are printed on the standard output or in specified files.

  If the input matrix has \code{NULL} as row names, and the
  \code{feat.name} option is left missing, a warning message will be
  print and the \code{NAME} column of the gct file will use integer
  indices starting from \code{1}.

  \code{feat.desc} specifies feature descriptions. Leaving is missing,
  or assigning it to \code{NA} or \code{NULL} will output a
  description column filled with empty strings.
}
\value{
  Texts printed in \code{stdout()} or in output file.
}
\note{
  From version 1.0-22, write_gct is able to handle zero-row matrix (see
  examples below)
}
\author{
    Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{read_gct_matrix}} to read matrix from GCT files.
}
\examples{
tmpMatrix <- matrix(rnorm(15), nrow=3L, ncol=5L,
dimnames=list(LETTERS[1:3L], letters[1:5L]))

write_gct(tmpMatrix)
write_gct(tmpMatrix, file=tempfile())

## specify feature names
write_gct(tmpMatrix, feat.name=c("F1", "F2", "F3"))
write_gct(tmpMatrix, feat.name=c("F1", "F2", "F3"), feat.desc=NULL)
write_gct(tmpMatrix, feat.name=c("F1", "F2", "F3"), feat.desc=NA)

## specify feature names and descriptions
write_gct(tmpMatrix, feat.name=c("F1", "F2", "F3"), feat.desc=
c("Feature 1", "Feature 2", "Feature 3"))

## special case: 0-row matrix
write_gct(tmpMatrix[c(FALSE,FALSE,FALSE),,drop=FALSE])
}

