\name{mroast2tbl}
\alias{mroast2tbl}
\title{
  Reformat mroast result lists into data frames.
}
\description{
  The \code{mroast} function in \code{limma} package returns a list as
  result. This function, \code{mroast2tbl}, formats the list into a
  \code{data.frame} object for the reporting purpose.
}
\usage{
mroast2tbl(mres)
}
\arguments{
  \item{mres}{A list, returned value of the \code{mroast} function}
}
\value{
  A \code{data.frame} object.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link[limma]{mroast}}
}
\examples{
if(require(limma)) {
  set.seed(1887)
  y <- matrix(rnorm(100*4),100,4)
  design <- cbind(Intercept=1,Group=c(0,0,1,1))
  iset1 <- 1:5
  y[iset1,3:4] <- y[iset1,3:4]+3
  iset2 <- 6:10
  mres <- mroast(list(set1=iset1,set2=iset2),y,design,contrast=2)
  mroast2tbl(mres)
}
}
