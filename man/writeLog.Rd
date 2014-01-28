\name{writeLog}
\alias{writeLog}
\alias{doLog}
\title{
  Write text as log to a connection
}
\description{
  Write text as log to a connection
}
\usage{
writeLog(fmt, ..., con = stdout(), level = 0)
doLog(fmt, ..., level=0)
}
\arguments{
  \item{fmt}{Format string to passed on to sprintf}
  \item{\dots}{Parameters passed on to sprintf}
  \item{con}{A connection, for instance a file (or its name) or \code{stdout()}}
  \item{level}{Logging level: each higher level will add one extra space
  before the message. See examples}
}
\details{
  The function \code{writeLog} can be used to log outputs and/or running
  status of scripts to \emph{one connection}. To use it one does \emph{not} need to run
  \code{registerLog} first.

  In contrast, \code{doLog} can be used to log on multiple connections
  that are registered by \code{registerLog}. Therefore, to register
  logger(s) with \code{registerLog} is a prerequisite of calling
  \code{doLog}. Internally \code{doLog} calls \code{writeLog}
  sequentially to make multiple-connection logging.
}
\value{
  Side effect is used.
}
\seealso{
  \code{registerLog} to register more than one loggers so that
  \code{doLog} can write to them sequentially.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
writeLog("This is the start of a log")
writeLog("Message 1", level=1)
writeLog("Message 1.1", level=2)
writeLog("Message 1.2", level=2)
writeLog("Message 2", level=1)
writeLog("Message 3", level=1)
writeLog("Message 3 (special)", level=4)
writeLog("End of the log");

## log with format
writeLog("This is Message \%d", 1)
writeLog("Square of 2 is \%2.2f", sqrt(2))

## NA is handled automatically
writeLog("This is a not available value: \%s", NA, level=1)
writeLog("This is a NULL value: \%s", NULL, level=1)
}
