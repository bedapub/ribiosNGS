\name{qqmsg}
\alias{qqmsg}
\title{Quitely Quit with Messages}
\description{Quitely quit R with messages}
\usage{
qqmsg(..., status = 0, save=FALSE, runLast = TRUE)
}
\arguments{
  \item{\dots}{Messages to be passed to \code{message}}
  \item{status}{Quit stats}
  \item{save}{Logical, should current working environment be saved?}
  \item{runLast}{Logical, should \code{.Last()} be executed?}
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{quit}}
}
\examples{
\dontrun{
qqmsg()
qqmsg("die", status=0)
qqmsg("Avada kedavra", status=-1)
qqmsg("Crucio!", "\n", "Avada kedavra", status=-100)
}
}
