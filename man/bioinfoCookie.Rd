\name{bioinfoCookie}
\alias{bioinfoCookie}
\title{Get Roche Bioinfo Cookie}
\description{
  Get Roche Bioinfo Cookie for user authenfication
}
\usage{
bioinfoCookie(force.refresh=FALSE)
}
\arguments{
  \item{force.refresh}{Logical. If set to \code{TRUE}, cached cookie is
    not used and a new cookie is fetched.}
}
\details{
  User name is obtained by querying the operating system.

  The function first checks whether a cached cookie exists: if so, it
  simply returns it without querying the webserver; otherwise, it
  fetches a newly backed cookie from the webserver.
}
\value{
  A character string representing the cookie
}
\references{
  \url{ http://bioinfo.bas.roche.com:8080/bios/specific/iwbi/appaccounts.html}
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
bioinfoCookie()
}
