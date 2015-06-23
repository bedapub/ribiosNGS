\name{parseNumVec}
\alias{parseNumVec}
\title{
  Parsing string or string vector into numeric vector
}
\description{
  Numeric vectors can be given as arguments in two ways: (1) separated
  blanks or (2) separated by other common separators, such as comma
  (,). This function parses a string, or a string vector into a numeric
  vector of expected length. In addition it is failure safe: user can
  specify the return value in case the parsing was not successful, 
}
\usage{
parseNumVec(str, expLen = 2, failVal = c(5, 5), sep=",")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{str}{A string or string vector, usually returned by the the
    \code{\link{getArg}} function.}
  \item{expLen}{Integer or NULL, expected length of the numeric
    vector. When set to NULL, the numeric vector can be of variable length.}
  \item{failVal}{If the parsing failed (for example length not correct,
    or non-numeric values were provided, this value will be returned}
  \item{sep}{Delimiter other than blank, by default the comma}
}
\details{
  The input value mostly comes from return values of the
  \code{\link{getArg}} function.
}
\value{
  A numeric vector of the expected length; in case of parsing failure,
  the \code{failVal} will be returned
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{getArg}}
}
\examples{
parseNumVec(c("2", "3", "5.5"), expLen=3, failVal=c(3,4,5))
parseNumVec("2,3,4", expLen=3, failVal=c(3,4,5))

## when the parameter was given as -opt 2,3, 4 (note the extra blank)
parseNumVec(c("2,3,", "4"), expLen=3, failVal=c(3,4,5))

## fail: length incorrect
parseNumVec(c("2", "3", "5.5"), expLen=4, failVal=c(3,4,5))

## success: length undetermined
parseNumVec(c("2", "3", "5.5"), expLen=NULL, failVal=c(3,4,5))

## fail: non-numeric value
parseNumVec(c("2", "3", "5.5", "TB"), expLen=4, failVal=c(3,4,5,5))
}

