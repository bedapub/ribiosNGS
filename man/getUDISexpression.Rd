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
"studyId", "studyTitle", "datasetId"))
}
\arguments{
  \item{id}{Character string, study identifier}
  \item{idType}{Character string, type of the identifier. By default the
  ID is treated as study ID external (\sQuote{studyIdExternal}); alternatives include SSIS internal
  study ID (\sQuote{studyId}), SSID study title (\sQuote{studyTitle}),  and dataset id \sQuote{datasetId}}
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
  See
  \url{http://bioinfo.bas.roche.com:8080/apps/ribios/ribiosUDIS-demo/ribiosUDIS-demo.html} for a demo.
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
myEset1_1 <- getUDISexpression(id="GSE20986")
myEset1_2 <- getUDISexpression(id="GSE20986", idType="studyIdExternal")

myEset2_1 <- getUDISexpression(id=1211, idType="datasetId")
myEset2_2 <- getUDISexpression(id=3084, idType="studyId")
myEset2_3 <- getUDISexpression(id="UUO_Mouse_Chugai", idtype="studyTitle")
myEset2_4 <- getUDISexpression(id="UUO_Mouse_Chugai", idtype="studyIdExternal")
}
}
