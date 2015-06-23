\name{checkFile}
\alias{checkFile}
\alias{assertFile}
\title{
  Check whether file(s) exist
}
\description{
  \code{checkFile} checks whether file exists, \code{assertFile} stops
  the program if files do not exist
}
\usage{
checkFile(...)
assertFile(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{Files to be checked}
}
\details{
  \code{assertFile} is often used in scripts where missing a file would
  cause the script fail.
}
\value{
  \code{checkFile} returns logical vector. \code{assertFile} returns an
  invisible \code{TRUE} if files exist, otherwise halts and prints error messages.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{isDir}} and \code{\link{assertDir}}
}
\examples{
myDesc <- system.file("DESCRIPTION", package="ribiosUtils")
myNEWS <- system.file("NEWS", package="ribiosUtils")
checkFile(myDesc, myNEWS)
assertFile(myDesc, myNEWS)
}

