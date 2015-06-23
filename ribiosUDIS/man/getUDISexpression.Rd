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
getUDISexpression(id = "GSE20986",idType=c("studyIdExternal",
"studyId", "studyTitle", "datasetId"), ...)
}
\arguments{
  \item{id}{Character string, study identifier}
  \item{idType}{Character string, type of the identifier. By default the
  ID is treated as study ID external (\sQuote{studyIdExternal}); alternatives include SSIS internal
  study ID (\sQuote{studyId}), SSID study title (\sQuote{studyTitle}),
  and dataset id \sQuote{datasetId}}
  \item{\dots}{Other parameter-value pairs to be passed to the API}
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

  If extra options are needed, \code{\dots} can be used. Since there are
  too many available parameters to parameterize, \code{dots} is very
  flexible in the sense that it accepts any pairs. The caller is
  responsible to check the options make sense. A complete description of
  the API options can be found at
  \url{http://udis.roche.com/apps/udisquery/apihelp.html}. Note that the
  options are only passed to the expression matrix and the probe
  (feature) matrix.

  See examples for a common use of \code{\dots}: to query the expression
  of one specific gene in interest. 
}
\value{
  An \code{ExpressionSet} object when the query succeeded, otherwise \code{NULL}.
}
\references{
  See
  \url{http://bioinfo.bas.roche.com:8080/apps/ribios/ribiosUDIS-demo/ribiosUDIS-demo.html} for a demo.

  UDIS API documentation
  \url{http://udis.roche.com/apps/udisquery/apihelp.html}. 
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\note{
  From version 1.1-0 the function points to the productive server of
  UDIS, and supports feature annotation.
}
\examples{
## here we might want to have a smaller dataset for demo
\dontrun{
myEset1_1 <- getUDISexpression(id="GSE20986")
myEset1_2 <- getUDISexpression(id="GSE20986", idType="studyIdExternal")

myEset2_1 <- getUDISexpression(id=1211, idType="datasetId")
myEset2_2 <- getUDISexpression(id=3084, idType="studyId")
myEset2_3 <- getUDISexpression(id="UUO_Mouse_Chugai", idtype="studyTitle")
myEset2_4 <- getUDISexpression(id="UUO_Mouse_Chugai",
idtype="studyIdExternal")

## an example to extract the expression of one gene (UCP1/7350) from a
large expression dataset (GTEx)
myEset3_1 <- getUDISexpression(id=3220, idType="studyId",
probeproptype="HUMANEG_ID", probeprop=7350)
}
}
