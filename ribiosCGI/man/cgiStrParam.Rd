\name{cgiStrParam}
\alias{cgiStrParam}
\title{Parse CGI parameter specifying string vectors}
\description{
  This function parses the CGI parameter that specify string vectors in
  R. 
}
\usage{
cgiStrParam(name, ignore.case = FALSE, default = NULL)
}
\arguments{
  \item{name}{Character, CGI parameter name}
  \item{ignore.case}{Logical, whether the parameter name matching should
    be case-independent}
  \item{default}{Character string or \code{NULL}, the default value returned in case the
    parameter was missing or the parsing was unsuccessful}
}
\details{
  The function attempts parse string vectors separated by plus signs
  (\code{+}) in URL into a vector of character strings. If fails, the \code{default} value is returned.

  A valid representation: \code{?col=1+2} and \code{?col=string1+string2}.
}
\value{
  A vector of colors that can be used in R functions.
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
cgiStrParam("col", ignore.case=FALSE, default=1L)
}}
