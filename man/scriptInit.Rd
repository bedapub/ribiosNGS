\name{scriptInit}
\alias{scriptInit}
\title{
  Prepare R for an interactive script
}
\description{
  Prepare R for an interactive script
}
\usage{
scriptInit(dumpto = "ribios.dump")
}
\arguments{
  \item{dumpto}{Dump file name in case of errors}
}
\details{
  The function prepares R for an interactive session (e.g. in a
  script). Currently it defines behaviour in case of errors. 
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
