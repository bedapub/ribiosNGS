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
  column.
}
\usage{
readFKtable(file, fk, strict.order=FALSE, ...)
}
\arguments{
  \item{file}{A table file.}
  \item{fk}{Characters, foreign keys.}
  \item{strict.order}{Logical, whether the foreign keys must have the
    same order as they appear in the file.}
  \item{\dots}{Other parameters passed to the \code{read.table} function.}
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

## try strict.order=TRUE
test.df <- data.frame(pts=c(15,14,13), plc=c("H", "G", "H"), row.names=rev(fk.teams))
write.table(test.df, test.file)
readFKtable(test.file, fk=fk.teams, strict.order=FALSE)
\dontrun{readFKtable(test.file, fk=fk.teams, strict.order=TRUE)}
}

