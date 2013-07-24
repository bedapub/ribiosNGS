\name{cgiColParam}
\alias{cgiColParam}
\title{Parse CGI parameter specifying colors}
\description{
  This function parses the CGI parameter that specify colors in R. It
  exploits the possibility to use either integers, or character strings
  to specify colors in R.
}
\usage{
cgiColParam(name, ignore.case = FALSE, default = 1L)
}
\arguments{
  \item{name}{Character, CGI parameter name}
  \item{ignore.case}{Logical, whether the parameter name matching should
    be case-independent}
  \item{default}{Character string or integer, the default color returned in case the
    parameter was missing or the parsing was unsuccessful}
}
\details{
  The function attempts parse integers that represent colors, or strings
  that specify colors. If fails, the \code{default} value is returned.

  Multiple colors can be separated by either comma (\code{,}) or plus
  signs (\code{+}). Both following representations are valid:
  \code{?col=1+2} and \code{?col=1,2}.
  
  In URLs, hash-code that is required by hexadecimal RGB colors
  is encoded by \code{\%23}. Therefore in URL the black color can be
  encoded as follows: \code{?col=\%23000000}.
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
cgiColParam("col", ignore.case=FALSE, default=1L)
}}
