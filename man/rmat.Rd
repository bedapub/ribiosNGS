\name{rmat}
\alias{rmat}
\title{Remove temporary files at a specified time interval from now}
\description{
  Send a \code{at} job to remove (probably temporary) files in the
  future with a specified time interval from now
}
\usage{
rmat(..., days = NULL, hours = NULL, minutes = NULL, dry = TRUE)
}
\arguments{
  \item{\dots}{Files to be removed}
  \item{days}{Numeric, interval in days}
  \item{hours}{Numeric, interval in hours}
  \item{minutes}{Numeric, interval in minutes}
  \item{dry}{Logical, if set to \code{TRUE}, only the command will be
    returned and files are not really removed.}
}
\details{
  The command will delete files, and there is usually no way to get deleted files back. \emph{So make sure you know
  what you are doing!}

  Days, hours, and minutes can be given in a mixed way: they will be
  summed up to give the interval.
}
\value{
  (Invisibly) the output of the \code{at} job.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\note{
  Since the command uses \code{at} internally, it is unlikely the
  command will work in the Windows system \dQuote{out of box}.
}

\seealso{
  \code{\link{qsystem}} for running system commands quietly.
}
\examples{
tmp1 <- tempfile()
tmp2 <- tempfile()
rmat(tmp1, tmp2, minutes=1)
}
