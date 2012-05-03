\name{getUDISexpression}
\alias{getUDISexpression}
\title{
  Get ExpressionSet from studies in UDIS Expression Database
}
\description{
  The function queries Expression Database of UDIS by study identifier,
  and returns an annotated \code{ExpressionSet} object.
}
\usage{
getUDISexpression(studyid = "NCS_tissue_rat")
}
\arguments{
  \item{studyid}{Character string, study identifier}
}
\details{
  The function first tries to get signals of the specified study. If it
  fails, for example due to an non-existing identifier or complications
  due to multiple platforms within one study, an error message will be
  printed and a \code{NULL} value will be returned. Otherwise, it further queries the phenotype data (known
  as \code{meta} in UDIS). Once both pieces are ready, the function
  builds an \code{ExpressionSet} object. 
}
\value{
  An \code{ExpressionSet} object when the query succeeded, otherwise \code{NULL}.
}
\references{
  \url{http://udisdev.roche.com/udiscgiqa/expressionData_cgi}
}
\author{
  Jitao David Zhang
}
\note{
  The function may need extensive testing, especially in the parsing of
  phenotype data. And the parsing of phenotype data may be optimized
  with C code when necessary in the future.

  Currently the feature/probesets annotation is not embedded. It will be
  implemented once UDIS has its own integrated annotation system.
}
\examples{
## here we might want to have a smaller dataset for demo
myEset <- getUDISexpression(studyid="NCS_tissue_rat")
}
