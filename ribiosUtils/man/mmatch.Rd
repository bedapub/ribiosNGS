\name{mmatch}
\alias{mmatch}
\title{
  Multiple matching
}
\description{
  Multiple matching between two vectors. Different from R-native
  \code{match} function, where only one match is returned even if there
  are multiple matches, \code{mmatch} returns all of them.
}
\usage{
mmatch(x, table, nomatch = NA_integer_)
}
\arguments{
  \item{x}{vector or \code{NULL}: the values to be matched.}
  \item{table}{vector or \code{NULL}: the values to be matched against.}
  \item{nomatch}{the value to be returned in case when no match is found.}
}
\details{
  Multiple matches can be useful in many cases, and there is no native
  R function for this purpose. User can write their own functions combining
  \code{lapplying} with \code{match} or \code{\%in\%}, our experience
  however shows that such non-vectorized function can be extremely slow,
  especially when the \code{x} or \code{table} vector gets longer.

  \code{mmatch} delegates the multiple-matching task to a C-level
  function, which is optimized for speed. Internal benchmarking shows
  improvement of hundred fold, namely using \code{mmatching} costs about
  1/100 of the time used by R-implementation.
}
\value{
  A list of the same length as the input \code{x} vector. Each
  list item contains the matching indices (similar to \code{match}).
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>, C-code was adapted
  from the program written by Roland Schmucki.
}
\seealso{
  \code{match}
}
\examples{
vec1 <- c("HSV", "BVB", "FCB", "HSV", "BRE", "HSV", NA, "BVB")
vec2 <- c("FCB", "FCN", "FCB", "HSV", "BVB", "HSV", "FCK", NA, "BRE", "BRE")

mmatch(vec1, vec2)

## compare to match
match(vec1, vec2)
}

