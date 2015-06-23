\name{scriptName}
\alias{scriptName}
\title{
  File name of the Rscript being executed
}
\description{
  Get the file name of the Rscript that is currently being executed. The
  function is mainly called by stand-alone Rscripts.

  The name is determined by the \code{--file}/\code{-f} option in the command line.
}
\usage{
scriptName()
}
\details{
  When the R session was not initiated by a Rscript (i.e. there is no
  \code{--file} or \code{-f} option in the command line), \code{NULL} is
  returned.

  Note that the function supports calling Rscript via \code{--file}
  or \code{-f} with \code{R}. This applies to cases where a Rscript,
  marked as executable, is called from the command line.
}
\value{
  A character string containing the file name of the Rscript. 
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{commandArgs}} and \code{\link{getArg}}
}
\examples{
\dontrun{scriptName()}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
