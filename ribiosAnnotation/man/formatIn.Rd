\name{formatIn}
\alias{formatIn}
\title{
  Formatting a vector for SQL SELECT query with IN syntax
}
\description{
  Prepare a vector for SQL SELECT query with the IN syntax
}
\usage{
formatIn(x)
}
\arguments{
  \item{x}{A vector to be queried with the IN syntax}
}
\value{
  A character string to be used after IN. See examples.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
myvec <- c("HH", "HM", "TH")
formatIn(myvec)
mysel <- "SELECT * FROM table WHERE city IN"
paste(mysel,formatIn(myvec))

}
