\name{write.tableList}
\alias{write.tableList}
\title{
  Write a list of data.frames (tables) into files
}
\description{A helpfer function to write a list of data frames into files}
\usage{
write.tableList(list, file.names, ...)
}
\arguments{
  \item{list}{A list of data frames}
  \item{file.names}{File names. If missing, the names of the list will
    be used. Must be of the same length as the list}
  \item{\dots}{Other parameters that are passed to \code{\link{write.table}}}
}
\value{
  Side-effects are used
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}

\seealso{
  \code{\link{write.table}}
}
\examples{
\dontrun{
df1 <- data.frame(name=c("A", "B", "C"), value=1:3)
df2 <- data.frame(name=c("C", "D", "E"), value=seq(9,3,-3))
dflist <- list(file1=df1, file2=df2)
write.tableList(dflist) ## two files, file1 and file2, are written
write.tableList(dflist, file.names=c("file1.txt", "file2.txt")) ## two
files, file1.txt and file2.txt, are written
}
}

