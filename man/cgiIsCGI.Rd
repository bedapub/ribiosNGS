\name{cgiIsCGI}
\alias{cgiIsCGI}
\title{Testing whether the program is called by the HTTP daemon}
\description{
  Returns a logical value indicating whether the program is called by
  the HTTP deamon.
}
\usage{
cgiInit()
}
\value{
  \code{TRUE} is the programming is running as a CGI backend,
  \code{FALSE} otherwise
}
\references{
  BIOS documentation of the html module: \url{http://bioinfo.bas.roche.com:8080/bios/common/libprpi/html.hdoc}
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{cgiInit}}
}
\examples{
\dontrun{
cgiIsCGI()
}
}

