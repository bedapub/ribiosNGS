\name{replaceColumnNames}
\alias{replaceColumnNames}
\title{
  Replace column names in data.frame
}
\description{
  Replace column names in data.frame
}
\usage{
replaceColumnNames(data.frame, old.names, new.names)
}
\arguments{
  \item{data.frame}{A data.frame}
  \item{old.names}{Old column names to be replaced}
  \item{new.names}{New column names}
}
\value{
  Data.frame with column names updated
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
clubs <- data.frame(Points=c(21,23,28,24), Name=c("BVB", "FCB", "HSV",
"FCK"), games=c(12,11,11,12))
replaceColumnNames(clubs, c("Points", "games"), c("Punkte", "Spiele"))
}
