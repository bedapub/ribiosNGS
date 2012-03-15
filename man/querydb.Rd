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
querydb(sqlComm, db = c("bia", "bin", "red"))
}
\arguments{
  \item{sqlComm}{A SQL query, a SELECT command.}
  \item{db}{Characters: which Oracle database should be connected?
    Currently \emph{bia}, \emph{bin} and \emph{red} are supported.}
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
\dontrun{
querydb("SELECT * FROM genome_sequence WHERE DB='HUMANN'", db="bin")
}
}

