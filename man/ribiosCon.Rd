\name{ribiosCon}
\alias{ribiosCon}
\title{
  Get a connection object
}
\description{
  Get a connection object for communication with Oracle database.
}
\usage{
ribiosCon(db = "bia", user = "biread", password = "biread", forceJDBC = FALSE)
}
\arguments{
  \item{db}{Database name}
  \item{user}{User name}
  \item{password}{Password}
  \item{forceJDBC}{Logical, forcing the user of JDBC interface to fetch
    data from Oracle servers. By default it is set to \code{FALSE}, since
    the JDBC interface is slower than native ROracle implementation. This
    option is largely for debugging purposes.}
}
\details{
  Depending on the availability of Oracle client library,
  \code{ribiosAnnotation} automatically uses either \code{OraConnection}
  of the \code{ROracle} package or \code{JDBCConnection} of \code{RJDBC}
  to access database.
}
\value{
  A connection object of the type \code{OraConnection} when
  Oracle client library is available, or \code{JDBCConnection}
  otherwise. Setting \code{forceJDBC} to \code{FALSE} makes the function
  return a \code{JDBCCOnnection} object independent of library availability.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{dbConnect}}
}
\examples{
options(error=utils::recover)
(con <- ribiosCon(db="bia", user="biread", password="biread"))
(conJDBC <- ribiosCon(db="bia", user="biread", password="biread",
forceJDBC=TRUE))

dbDisconnect(con)
dbDisconnect(conJDBC)
options(error=NULL)
}
