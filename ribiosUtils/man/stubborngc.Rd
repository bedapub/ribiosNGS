\name{stubborngc}
\alias{stubborngc}
\title{Repeat garbage-collecting until all resource is freed}
\description{
  stubborngc repeats collecting garbage untill no more resource can be freed
}
\usage{
stubborngc(verbose = FALSE, reset = TRUE)
}
\arguments{
  \item{verbose}{Logical, verbose or not}
  \item{reset}{Logical, reset or not.}
}
\value{
  Side effect is used.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{gc}}
}
\examples{
stubborngc()
}
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
