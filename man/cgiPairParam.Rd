\name{cgiPairParam}
\alias{cgiPairParam}
\title{Parse CGI parameter specifying key-value pairs}
\description{
  This function parses the CGI parameter that specifies key-value pairs
}
\usage{
  cgiPairParam(name, ignore.case=FALSE,
               default=data.frame(keys=character(),values=character()),
               sep=":", collapse=" ")
}
\arguments{
  \item{name}{Character, CGI parameter name}
  \item{ignore.case}{Logical, whether the parameter name matching should
    be case-independent}
  \item{default}{A data.frame or \code{NULL}, the default value returned in case the
    parameter was missing or the parsing was unsuccessful}
  \item{sep}{Separator used between key and value in each pair.}
  \item{collapse}{Separator used between key-value pairs}
}
\details{
  The function attempts to parse key-value pairs. If fails, the \code{default} value is returned.

  By default the \code{collapse} paramter is set as a blank character,
  which corresponds to as a plus sign (\code{+}) in the URL. Therefore,
  using the default \code{sep} and \code{collapse}, a valid URL looks as
  follows: \code{?vals=key1:val1+key2:val2+key3+val3}
}
\value{
  A \code{data.frame} with two columns: \code{keys} and \code{values}
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
cgiPairParam("vals", ignore.case=FALSE)
}}
