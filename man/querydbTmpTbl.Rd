\name{querydbTmpTbl}
\alias{querydbTmpTbl}
\title{Query database with single-column temporary table}
\description{
  Low-level function to query database with filling single-column
  temporary table with user inptu values.
}
\usage{
querydbTmpTbl(sqlComm, inCol, inValues, db = "bia", user = "biread", password = "biread")
}
\arguments{
  \item{sqlComm}{Character string of SQL command. The command can be
    written normally as if no temporary table is used.}
  \item{inCol}{Which column stated in the SQL command should be joined
    with the temporary table values?}
  \item{inValues}{Character vector, values to be filled into the
    temporary table}
  \item{db}{Database}
  \item{user}{User name}
  \item{password}{User password}
}
\details{
  This function is intended to be used by developer or advanced users
  who wish to access the database directly. End-users should not use
  this function unless 100\% clear what he or she is doing.
  
  This function uses temporary table to perform query with user
  input. This can be alternatively done with
  \code{\link{querydbSelectIn}}. Howevever \code{\link{querydbSelectIn}}
  has the limitation of 1000 elements per SQL query, and the current
  implementation (splitting input values in 1000-element blocks) can be
  less efficient compared to \code{queryTmpTbl}.
}
\value{
  A \code{data.frame} depending on the query.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{querydbSelectIn}}
}
\examples{
options(error=utils::recover)
hcIn <- querydbTmpTbl("SELECT * FROM genome_sequence WHERE DB='HUMANN' ",
                      inCol="SEQ", inValues=c("CHR1", "CHR5", "CHRX"),
                      db="bin", user="genome", password="genome")
options(error=NULL)
}
