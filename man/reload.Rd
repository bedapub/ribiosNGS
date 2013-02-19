\name{reload}
\alias{reload}
\title{
  Reload a package
}
\description{
  Reload a package by first detaching and loading the library.
}
\usage{
reload(pkg)
}
\arguments{
  \item{pkg}{Character string, name of the package}
}
\value{
  Side effect is used.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
\code{\link{detach}} and \code{\link{library}}
}
\examples{
\dontrun{
library(ribiosUtils)
reload(ribiosUtils)
}
}
