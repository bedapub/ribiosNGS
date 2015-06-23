\name{cgiHeader}
\alias{cgiHeader}
\title{Print HTML header of type}
\description{
  The function prints HTML header of type, but only if running under
  WWW. Possible values include \code{text/plain}, \code{text/html}, and
  \code{image/png}, etc. 
}
\usage{
cgiHeader(header)
}
\arguments{
  \item{header}{HTML header, character}
}
\value{
  Side effect is used.
}
\references{
  BIOS documentation of the html module: \url{http://bioinfo.bas.roche.com:8080/bios/common/libprpi/html.hdoc}
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{cgiInit}}, \code{\link{cgiParams}}
}
\examples{
\dontrun{
cgiHeader()
}
}

