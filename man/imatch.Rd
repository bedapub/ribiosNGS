\name{imatch}
\alias{imatch}
\alias{imatchv}
\alias{ipmatch}
\alias{ipmatchv}
\alias{matchv}
\title{
  Case-insensitive match and pmatch
}
\description{
  Case-insensitive \code{match} and \code{pmatch} functions, especially
  useful in parsing user inputs, e.g. from command line.
}
\usage{
matchv(x, table, ...)
imatch(x, table, ...)
imatchv(x, table, ...)
ipmatch(x, table, ...)
ipmatchv(x, table, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{String vector}
  \item{table}{A vector to be matched}
  \item{\dots}{Other parameters passed to \code{match} or \code{pmatch}}
}
\details{
  \code{imatch} and \code{ipmatch} works similar as \code{match} and
  \code{pmatch}, except that they are case-insensitive.

  \code{matchv}, \code{imatchv} and \code{ipmatchv} are shot-cuts to get
  the matched value (therefore the \sQuote{v}) if the match succeeded,
  or \code{NA} if not. \code{match(x, table)} is equivalent to
  \code{table[match(x, table)]}. See examples.
}
\value{
  \code{imatch} and \code{ipmatch} returns matching indices, or
  \code{NA} (by default) if the match failed.

  \code{matchv}, \code{imatchv} and \code{ipmatchv} returns the
  matching element in \code{table}, or \code{NA} if the match
  failed. Note that when cases are different in \code{x} and
  \code{table}, the one in \code{table} will be returned. This is
  especially useful for cases where user's input has different cases as
  the internal options.
}

\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{match}} and \code{\link{pmatch}}
}
\examples{
user.input <- c("hsv", "BvB")
user.input2 <- c("HS", "BV")
internal.options <- c("HSV", "FCB", "BVB", "FCN")

match(user.input, internal.options)
imatch(user.input, internal.options)
ipmatch(user.input, internal.options)
ipmatch(user.input2, internal.options)

matchv(user.input, internal.options)
matchv(tolower(user.input), tolower(internal.options))
imatchv(user.input, internal.options)
ipmatchv(user.input, internal.options)
ipmatchv(user.input2, internal.options)
}
