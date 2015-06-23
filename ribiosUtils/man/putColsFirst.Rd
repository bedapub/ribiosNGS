\name{putColsFirst}
\alias{putColsFirst}
\title{
  Rearrange columns to put some columns to far left
}
\description{
  This function is helpful to export tables where certain columns are
  desired to be placed to the most left of the data.frame
}
\usage{
putColsFirst(data.frame, columns)
}
\arguments{
  \item{data.frame}{Data.frame}
  \item{columns}{Character vector, names of columns which are to be put
    to the left}
}
\value{
  data.frame with re-arranged columns
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
clubs <- data.frame(Points=c(21,23,28,24), Name=c("BVB", "FCB", "HSV",
"FCK"), games=c(12,11,11,12))
putColsFirst(clubs, c("Name"))
putColsFirst(clubs, c("Name", "games"))
}

