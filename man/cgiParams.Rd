\name{cgiParams}
\alias{cgiParams}
\title{Get key-value parameter pairs from CGI}
\description{
  The function get key-value parameter pairs from CGI.
}
\usage{
cgiParams()
}
\value{
  A named vector of characters: the names are keys, and the character
  strings are the values.
}
\references{
  BIOS documentation of the html module: \url{http://bioinfo.bas.roche.com:8080/bios/common/libprpi/html.hdoc}
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{cgiInit}}, \code{\link{cgiHeader}}
}
\examples{
\dontrun{
cgiParams()
}
}

