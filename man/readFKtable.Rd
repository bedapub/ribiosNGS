\name{readFKtable}
\alias{readFKtable}
\title{Read table with Foreign Keys}
\description{
  The concept \code{Foreign Keys} comes from relational databse
  systems. These keys can be used to cross-reference tables. Say we have
  two \code{data.frames}, one contains gene annotations and the other
  contains protein annotations. A column named \code{mRNArefseqID} may
  be the foreign key that can be used to specify relationships between
  gene and proteins.

  The \code{readFKtable} reads a table from file, and checks if it
  contains provided foreign keys: either as row.names or in the first
  column. An important difference here from databases is that the order
  of the foreign keys must be exactly the same in the file, there will
  be no automatical matching done.
}
\usage{
readFKtable(file, fk, ...)
}
\arguments{
  \item{file}{A table file}
  \item{fk}{Characters, foreign keys}
  \item{\dots}{Other parameters passed to the \code{read.table} function}
}
\value{
  A \code{data.frame} if the FK-matching was successful, otherwise the
  function will print an error message and stop.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
test.file <- tempfile()
fk.teams <- c("HSV", "FCB", "BVB")
## FK in row names
test.mat <- matrix(rnorm(9), nrow=3, dimnames=list(fk.teams, NULL))
write.table(test.mat, test.file)
readFKtable(test.file, fk=fk.teams)

## or: FK can be in the first column
test.df <- data.frame(team=fk.teams, pts=c(15,14,15),plc=c("H", "G", "H"))
write.table(test.df, test.file)
readFKtable(test.file, fk=fk.teams)
}

