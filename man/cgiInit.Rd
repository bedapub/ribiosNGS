\name{cgiInit}
\alias{cgiInit}
\title{Initialize CGI module}
\description{
  Initialize the BIOS CGI module. Within a CGI program, this function
  should be called as early as possible. 
}
\usage{
cgiInit()
}
\value{
  No value is returned.
}
\references{
  BIOS documentation of the html module: \url{http://bioinfo.bas.roche.com:8080/bios/common/libprpi/html.hdoc}
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{cgiHeader}}, \code{\link{cgiParams}}
}
\examples{
\dontrun{
cgiInit()
}
}

