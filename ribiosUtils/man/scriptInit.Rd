\name{scriptInit}
\alias{scriptInit}
\title{
  Prepare R for an interactive script
}
\description{
  Prepare R for an interactive script
}
\usage{
scriptInit()
}
\details{
  The function prepares R for an interactive session (e.g. in a
  script). Currently it defines behaviour in case of errors: a file
  named \dQuote{ribios.dump} is written.
}
\value{
  Side effect is used.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{options}}
}
\examples{
\dontrun{
scriptInit()
}
}
