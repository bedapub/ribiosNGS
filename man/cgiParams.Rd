\name{cgiParams}
\alias{cgiParams}
\title{Get key-value parameter pairs from CGI}
\description{
  The function get key-value pairs of CGI parameters as a named character vector.
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
  For preceeding operations, see \code{\link{cgiInit}},
  \code{\link{cgiGet2Post}}, and \code{\link{cgiHeader}}. To get the
  value of one particular parameter, see \code{\link{cgiParam}}.
}
\examples{
\dontrun{
cgiInit()
cgiGet2Post()
cgiHeader("text/html")
cgiParams()
}
}

