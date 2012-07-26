\name{hasOracle}
\alias{hasOracle}
\title{
  Testing whether the Oracle client library is installed
}
\description{
  The function tests whether the Oracle client library is installed. If
  so, the package uses \code{ROracle} as driver interface to Oracle
  database, otherwise \code{RJDBC} is used.
}
\usage{
hasOracle()
}
\value{
  Logical. \code{TRUE} indicates Oracle SQL client is installed.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
hasOracle()
}