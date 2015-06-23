\name{list2mat}
\alias{list2mat}
\title{Convert a first-level list into adjacency matrix}
\description{
  Convert a first-level list into adjacency matrix
}
\usage{
list2mat(list)
}
\arguments{
  \item{list}{A first-level list. See details}
}
\details{
  First-level list must have vectors of basic data types defined by R
  such as \code{characater}, \code{integer}, \code{number}, and
  \code{logical}.The function transforms such a list into adjacency
  matrix, rows of which are vector elements and columns of which are
  names of the list.
}
\value{
  An adjacency matrix. Row and column names are defined by unique elements and list
  names, respectively.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
testList <- list(HSV=c("Adler", "Westermann", "Jansen"), FCB=c("Robben",
"Jansen", "Neuer"), S04=c("Westermann", "Neuer"))
list2mat(testList)

testList2 <- list(c("A", "B", "C"), c("B", "C", "D"), c("D", "E", "F"))
list2mat(testList2)

testList3 <- list(Worker1=0:8L, Worker2=5:13L, Worker3=8:16L, Worker4=16:24L)
list2mat(testList3)
}
