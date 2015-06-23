\name{isDir}
\alias{isDir}
\alias{checkDir}
\alias{assertDir}
\title{
  Checks existing directory
}
\description{
  Checks whether given character strings point to valid directories
}
\usage{
isDir(...)
checkDir(...)
assertDir(...)
}
\arguments{
  \item{\dots}{One or more character strings giving directory names to
    be tested}
}
\details{
  \code{isDir} tests whether the given string represent a valid,
  existing directory. \code{assertDir} performs a logical test, and
  stops the program if the given string does not point to a given
  directory.

  \code{checkDir} is synonymous to \code{isDir}
}
\value{
  \code{isDir} returns logical vector.

  \code{assertDir} returns an invisible \code{TRUE} if directories exist, otherwise halts and prints error messages.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{file.info}}, \code{\link{checkFile}} and \code{\link{assertFile}}
}
\examples{
dir1 <- system.file(package="ribiosIO")
dir2 <- system.file(package="ribiosUtils")

isDir(dir1, dir2)
\dontrun{assertDir(dir1, dir2)}
}
