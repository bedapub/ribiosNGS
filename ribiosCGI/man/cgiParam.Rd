\name{cgiParam}
\alias{cgiParam}
\title{Get value of a given parameter}
\description{
  The function get the value of a given CGI parameter.
}
\usage{
cgiParam(name, ignore.case=FALSE, default=NULL)
}
\arguments{
  \item{name}{Parameter name to be queried}
  \item{ignore.case}{Perform case-insensitive name-matching or not,
    Logical value}
  \item{default}{Value returned if the parameter was not found}
}
\details{
  If \code{ignore.case} is specified, the parameter name matching is
  case-insensitive. Otherwise (and by default) the match is case-sensitive.
}
\value{
  If the parameter is found, its value (a charactering string) is
  returned. If not found, the value of \code{default} is returned.
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
  key-value pairs of all parameters, see \code{\link{cgiParams}}.
}
\examples{
\dontrun{
cgiInit()
cgiGet2Post()
cgiHeader("text/html")
cgiParams()
}
}

