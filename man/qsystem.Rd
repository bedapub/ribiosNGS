\name{qsystem}
\alias{qsystem}
\title{
  Quietly runs a system command
}
\description{
  Quietly runs a system command: the output is internalized and returned
  as an invisible variable, and the standard error output is ignored.
}
\usage{
qsystem(command)
}
\arguments{
  \item{command}{A system command}
}
\details{
  The function runs the system command in a quiet mode. The function can
  be useful in CGI scripts, for instance
}
\value{
  (Invisibly) the internalized output of the command
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}

\examples{
dateIntern <- system("date")
}

