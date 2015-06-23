\name{cgiFuncParam}
\alias{cgiFuncParam}
\title{Parse CGI parameter specifying R function}
\description{
  This function parses the CGI parameter that specifies a R-function,
  either as a known function name or in definition.
}
\usage{
cgiFuncParam(name, ignore.case = FALSE, default = NULL)
}
\arguments{
  \item{name}{Character, CGI parameter name}
  \item{ignore.case}{Logical, whether the parameter name matching should
    be case-independent}
  \item{default}{Character string, default value: it should be either
    NULL or a R function.}
}
\details{
  The function attempts parse the CGI parameter that represents either a
  known R function (e.g. \code{?func=sqrt}), or a inline defined
  function (e.g. \code{?func=function(x) x/5}). If failed, the
  \code{default} value is returned.

  The function will stop and print error message if the parameter value
  is not a valid R function, or the function name it points to cannot be
  found.
  
  In URL, plus signs (\sQuote{+}) in function body has to be encoded as
  \code{\%2B}.
}
\value{
  A function, or the value specified by \code{NULL}. If parsed
  succesfully, the object has an attribute \code{label} as its text
  representation: either the function name (in case of pre-defined
  functions), or the function body (in case of inline defined functions).
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
cgiFuncParam("func", ignore.case=FALSE, default=function(x) x)
}}
