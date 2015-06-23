\name{libordie}
\alias{libordie}
\title{
  Load a library mutedly and quit (die) in case of failing
}
\description{
  The specified library is loaded mutedly by suppressing all
  messages. If the library is not found, or its version under the
  specification of \code{minVer}, the R session dies with a message.
}
\usage{
libordie(package, minVer, missing.quit.status = 1, ver.quit.status = 1, ...)
}
\arguments{
  \item{package}{One package name (can be character or non-quoted
    symbol (see examples)}
  \item{minVer}{Optional, character string, the minimum working version}
  \item{missing.quit.status}{Integer, the status of quitting when the package was
  not found}
  \item{ver.quit.status}{Integer, the status of quitting when the
    package was found, but older than the minimum working version}
  \item{\dots}{Other parameters passed to the \code{\link{library}} function}
}
\details{
  Only one package should be tested once.
}
\value{
  \code{NULL} if success, otherwise the session will be killed.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  The function calls \code{\link{qqmsg}} internally to kill the session
}
\examples{
\dontrun{
libordie(stats)
libordie("methods")
libordie(base, minVer="2.15-1")
}
}

