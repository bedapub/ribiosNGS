\name{loadFile}
\alias{loadFile}
\title{Attempt to load a binary RData file}
\description{
  The function attempts to load a binary file, returning \code{TRUE} if
  succeeded. Otherwise it returns \code{FALSE}.
}
\usage{
loadFile(rDataFile, env = globalenv())
}
\arguments{
  \item{rDataFile}{Character, RData file name}
  \item{env}{Environment, where should be the RData loaded
    into. By default it is loaded into the global environment.}
}
\value{
  The function is used for its side effects.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{iofile}} can be used to find file from input data directory.
}
\examples{
\dontrun{
rf <- tempfile()
myData <- c(3,4,5)
save(myData, file=rf)
if(!loadFile(rf)) {
stop("Something went wrong\n")
}
}
}
