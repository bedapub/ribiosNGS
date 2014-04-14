\name{createDir}
\alias{createDir}
\title{
  Create a directory if it does not exist, and then make sure the
  creation was successful. 
}
\description{
  The function is particularly useful for scripting.
}
\usage{
createDir(dir, showWarnings = FALSE, recursive = FALSE, mode = "0777")
}
\arguments{
  \item{dir}{Directory name}
  \item{showWarnings}{Passed to \link{dir.create}}
  \item{recursive}{Passed to \link{dir.create}}
  \item{mode}{Passed to \link{dir.create}}
}
\value{
  Side effect is used. 
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
tempdir <- tempdir()
createDir(tempdir)
}
