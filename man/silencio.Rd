\name{silencio}
\alias{silencio}
\title{
  Keep silent by suppressing warnings and messages
}
\description{
  The function is used to keep the command silent by suppressing
  warnings and messages
}
\usage{
silencio(...)
}
\arguments{
  \item{\dots}{Any function call}
}
\value{
  The same as the function call
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{suppressWarnings}},   \code{\link{suppressMessages}}
}
\examples{
wsqrt <- function(x) {warning("Beep");message("Calculating square");return(x^2)}
\dontrun{wsqrt(3)}
silencio(wsqrt(3))
}

