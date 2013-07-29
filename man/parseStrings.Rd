\name{parseStrings}
\alias{parseStrings}
\title{Parse collapsed multiple options into a vector of character strings}
\description{
  This function parses collapsed multiple options into a vector of
  character strings. Each option is optionally trimmed of leading and tailing empty
  spaces given by \code{trim}. See examples.
}
\usage{
parseStrings(str, collapse = ",", trim=TRUE, ...)
}
\arguments{
  \item{str}{Character, input string.}
  \item{collapse}{Character, separators used between multiple options}
  \item{trim}{Logical, whether individual options should be trimmed}
  \item{\dots}{Parameters passed on to \code{trim}}
}
\details{
  In case of multiple separators, they can be given by concatenating
  with piple signs, e.g. \code{,|\\t}.
}
\value{A vector of character strings}
\author{Jitao David Zhang}
\seealso{\code{\link{strsplit}}, \code{\link{trim}}}
\examples{
parseStrings("a,b,c")

## options are trimmed
parseStrings("a,b,\tc,d\n")

## it works also with only one option
parseStrings("a")

## more than one separators
parseStrings("a,b,c;d", collapse=",|;")
}
