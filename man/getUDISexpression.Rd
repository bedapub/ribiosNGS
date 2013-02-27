\name{getUDISexpression}
\alias{getUDISexpression}
\title{
  Get ExpressionSet from studies in UDIS Expression Database
}
\description{
  The function queries Expression Database of UDIS by external study identifier,
  and returns an annotated \code{ExpressionSet} object.
}
\usage{
getUDISexpression(studyIdExt = "GSE20986")
}
\arguments{
  \item{studyIdExt}{Character string, study identifier}
}
\details{
  The function first tries to get signals of the specified study. If it
  fails, for example due to an non-existing identifier or complications
  due to multiple platforms within one study, an error message will be
  printed and a \code{NULL} value will be returned.

  If signals are successfully acquired, it further queries feature
  annotation and phenotype data (known as \code{meta} data in
  UDIS). Once all pieces are ready, the function
  builds and returns an \code{ExpressionSet} object. 
}
\value{
  An \code{ExpressionSet} object when the query succeeded, otherwise \code{NULL}.
}
\references{
  \url{http://udis.roche.com/apps/expression/api_help2.html}
}
\author{
  Jitao David Zhang
}
\note{
  From version 1.1-0 the function points to the productive server of
  UDIS, and supports feature annotation.
}
\examples{
## here we might want to have a smaller dataset for demo
\dontrun{
myEset <- getUDISexpression(studyIdExt="GSE20986")
}
}
