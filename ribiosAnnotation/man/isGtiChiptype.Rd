\name{isGtiChiptype}
\alias{isGtiChiptype}
\title{Logical function of whether input chiptype is supported by GTI}
\description{
  Given a character vector of chiptypes, the function returns a logical
  (boolean ) vector of the same length, each value indicating whether
  the input chiptype is supported by GTI. Not rarely the vector is of
  length one.

  It is possible to specify exceptions and case-insensitive
  comparisons. See details below
}
\usage{
isGtiChiptype(x, exceptions, ignore.case = FALSE)
}
\arguments{
  \item{x}{Mandatory, a character vector of chip types (names) to be checked}
  \item{exceptions}{Optional, a character vector of extended list. When
    an input chip type is included in \code{exceptions}, the
    corresponding returning value will be set as \code{TRUE}}
  \item{ignore.case}{Optional,logical. If set to \code{TRUE}, the
    comparison is performed in a case-insensitive way.}
}
\details{
  The function can be used to test whether user input is supported by
  GTI, ensuring the probesets or features of array can be properly
  annotated.

  \code{exceptions} allow an extended list to be checked against. For
  instance, currently (Feb 2012) \code{GeneID} is not one of the official
  GTI chiptypes. However, if the software should think \code{GeneID} is
  also a valid input, \code{exceptions} can be set as \dQuote{GeneID}.
}
\value{
  A logical vector of the same length as the input, indicating whether the input
  types are supported by GTI (\code{TRUE}) or not (\code{FALSE}).
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  The function calls internally the \code{\link{gtiChiptypes}} function
  to get a list of GTI-supported chiptypes.
}
\examples{
\dontrun{
user.input <- c("HG-U133A", "HG-U133_PLUS_2", "HUMAN_REFSEQ-8")
isGtiChiptype(user.input)

## exceptions
user.extinput <- c(user.input, "GeneID", "GeneSymbol", "RefSeq")
isGtiChiptype(user.extinput)
isGtiChiptype(user.extinput, exceptions=c("GeneID", "RefSeq"))

## case-insensitive comparisons
isGtiChiptype(tolower(user.input))
isGtiChiptype(tolower(user.input), ignore.case=TRUE)
}
}
