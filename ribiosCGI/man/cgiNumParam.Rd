\name{cgiNumParam}
\alias{cgiNumParam}
\title{Parse CGI parameter specifying numbers}
\description{
  This function parses the CGI parameter that specify numbers in R. It
  can accept most commonly representation of numbers (integers, real values, and in
  scientific presentation), as a single value or in a vector separated
  by commas or plus signs in URL.
}
\usage{
cgiNumParam(name, ignore.case = FALSE, expLen=NULL, default = NULL)
}
\arguments{
  \item{name}{Character, CGI parameter name}
  \item{ignore.case}{Logical, whether the parameter name matching should
    be case-independent}
  \item{expLen}{Integer, specifying the expected length of the
    vector. If NULL, vector of any length is accepted.}
  \item{default}{Numeric, the default value returned in case the
    parameter was missing or the parsing was unsuccessful}
}
\details{
  The function attempts parse numbers. If fails, the \code{default} value is returned.
  
  Multiple numbers can be separated by either comma (\code{,}) or plus
  signs (\code{+}). Therefore both following representations are valid:
  \code{?number=1+2} and \code{?number=1,2}.

  If \code{expLen} is not \code{NULL} and the parsed input vector has a
  different length, the default value is returned.
}
\value{
  A vector of numbers
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
cgiNumParam("numbers", ignore.case=FALSE, expLen=NULL, default=1L)
}}
