\name{cgiCateParam}
\alias{cgiCateParam}
\alias{cgiEnumParam}
\title{Parse CGI parameter specifying categorical variables}
\description{
  This function parses the CGI parameter that specifies categorical variables in R.
}
\usage{
cgiCateParam(name, ignore.case = FALSE, allowed.values=NULL, default = NULL)
}
\arguments{
  \item{name}{Character, CGI parameter name}
  \item{ignore.case}{Logical, whether the parameter name matching should
    be case-independent}
  \item{allowed.values}{Character vector, allowed values. It should not
    contain \code{NULL}, see details below.}
  \item{default}{Character string, default value: it must be either
    \code{NULL} or a member in the \code{allowed.value} vector.}
}
\details{
  The function attempts parse the CGI parameter that represent a
  categorical parameter (also known as the the enum type). If fails, the
  script is halted and error message is printed.

  Allowed values are specified in the vector of
  \code{allowed.values}, which should not contain the value
  \code{NULL}. If the \code{default} parameter is set to \code{NULL},
  then the returning value of the function will be \code{NULL} if the
  requested parameter was not set; otherwise, if the \code{default}
  parameter is not \code{NULL}, it must be one of the values in
  \code{allowed.values}. In the latter case, the function returns the
  default value.
}
\value{
  A value in the \code{allowed.values} vector, or \code{NULL} iif
  \code{default} was set to \code{NULL} and the parameter was not given
  by the client.
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
cgiCateParam("cate", ignore.case=FALSE, allowed.values=c("a", "b", "c"),
default=NULL)
}}
