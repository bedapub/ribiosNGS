\name{parsePairs}
\alias{parsePairs}
\title{Parse key-value pairs}
\description{Parse key-value pairs}
\usage{
parsePairs(str, collapse = ",", sep = "=", colnames = c("key", "value"),
trim = TRUE, ...)
}
\arguments{
  \item{str}{String, parameters in key-value pairs}
  \item{collapse}{Character, character(s) used to separate pairs}
  \item{sep}{Character, character(s) used to separate key and value}
  \item{colnames}{A character vector of length two, column names of returning values}
  \item{trim}{Logical, whether keys and values should be trimmed.}
  \item{...}{Other parameters passed to \code{trim}}
}
\details{
  The function parses parameters in the form of
  \code{KEY1=VAL1,KEY2=VAL2,KEY3=VAL3} into \code{data.frame}.

  
  If input string is \code{NULL}, the function returns \code{NULL}. This
  can be useful in case the parameter is optional and not specified.

}
\value{
  A two-column \code{data.frame}
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{parseStrings}
}
\examples{
parsePairs("A=3,B=2,C=1")

## no trimming
parsePairs("A=\t,B=2,C=1", trim=TRUE) ## \t is trimmed
parsePairs("A=\t,B=2,C=1", trim=FALSE) ## \t is kept
}
