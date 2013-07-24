\name{cgiLogiParam}
\alias{cgiLogiParam}
\title{Parse logical CGI parameter}
\description{
  This function parses CGI parameter into a logical value
}
\usage{
cgiLogiParam(name, ignore.case = FALSE, default = FALSE)
}
\arguments{
  \item{name}{Character, CGI parameter name}
  \item{ignore.case}{Logical, whether the parameter name matching should
  be case-independent}
  \item{default}{Logical, the default value returned in case the
    parameter was missing or the parsing was unsuccessful}
}
\details{
  The function attempts to convert the parameter value into a logical
  value in R. If fails, the \code{default} value is returned.
}
\value{
  A logical value of \code{TRUE} or \code{FALSE}
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  The function is based on \code{\link{cgiParam}}.
}
\examples{
\dontrun{
cgiInit();
cgiGet2Post()
cgiHeader("text/html")
cgiLogiParam("verbose", ignore.case=FALSE, default=FALSE)
}}
