\name{querydbSelectIn}
\alias{querydbSelectIn}
\title{
  Query database by selecting with SQL IN syntax
}
\description{
  The function builds SELECT SQL commands with IN syntax. By default the
  Oracle SQL server supports IN syntax up to 1000 items. The function handles
  such cases by splitting the query. Therefore it is capable to
  let users perform SELECT operation with IN syntax without worrying
  about the number of items.
}
\usage{
querydbSelectIn(sqlComm, inCol, inValues, db = "bia", user = "biread", password = "biread")
}
\arguments{
  \item{sqlComm}{Character string. It must be a \sQuote{SELECT} clause,
    and should be complete without the \dQuote{IN (sQuote{...})}
    part. See example below.}
  \item{inCol}{Character string, the input items are in which column?}
  \item{inValues}{Character vector, input items}
  \item{db}{Database}
  \item{user}{User name}
  \item{password}{User password}
}
\details{
  The function builds SELECT SQL command with IN syntax, and splits the
  database query if needed. This is necessary when there are over 1000
  IN items.
}
\value{
  As \code{\link{querydb}}, a \code{data.frame} of the query results.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{querydb}} to query databases.
}
\examples{
options(error=utils::recover)
querydbSelectIn(sqlComm="SELECT * FROM genome_sequence WHERE DB='HUMANN' AND ",inCol="SEQ", inValues=c("CHR1", "CHR5", "CHR16", "CHRY"),
db="bin", user="genome", password="genome")

options(error=NULL)
}
