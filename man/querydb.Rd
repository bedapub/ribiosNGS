\name{querydb}
\alias{querydb}
\title{
  Query database instances with prepared SQL commands
}
\description{
  The function takes one SQL command (probably a SELECT query) and
  returns the results. It takes care of database connection, sending
  queries, fetching results, and disconnection. 
}
\usage{
querydb(sqlComm, db = "bia", user = "biread", password = "biread")
}
\arguments{
  \item{sqlComm}{A SQL query, a SELECT command.}
  \item{db}{Characters, which Oracle database should be connected?}
  \item{user}{Characters, user name for login}
  \item{password}{Characters, password for login}
}
\details{
  The \emph{sqlComm} parameter must not end with comma (;).
  
  The function stops at SQL error by printing error messages.
}
\value{
  A data frame queried by the user
}
\references{
  \url{http://bioinfo.bas.roche.com:8080/bioracle/}
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  Internally the function calls following comamnds: \code{dbSendQuery},
  \code{dbClearResult} and \code{dbDisconnect}.
}
\examples{
options(error=utils::recover)
querydb("SELECT * FROM genome_sequence WHERE DB='HUMANN'", db="bin",
user="genome", password="genome")
options(error=NULL)
}

