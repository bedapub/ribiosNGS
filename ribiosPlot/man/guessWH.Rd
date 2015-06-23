\name{guessWH}
\alias{guessWH}
\title{
  Guess width and height parameters for a heatmap
}
\description{
  guessWH helps \code{biosHeatmap} determines a proper canvas dimension
  as well as the proportion between legends and the main figure,
  especially in the command line mode.
}
\usage{
guessWH(nrow, ncol, rownames, colnames, cexRow, cexCol, xlab, ylab, width, height)
}
\arguments{
  \item{nrow}{Row count of the matrix to be visualized}
  \item{ncol}{Column count of the matrix to be visualized}
  \item{rownames}{Row names of the matrix, helping to determine
    horizontal margins. Can be missing or set as \code{NULL}}
  \item{colnames}{Column names of the matrix, helping to determine
    vertical margins. Can me missing or set as \code{NULL}}
  \item{cexRow}{Row name font size. Can be missing or \code{NA}.}
  \item{cexCol}{Column name font size. Can be missing or \code{NA}.}
  \item{xlab}{X-axis (column side) name. Character string. Can be
    missing or \code{NA}}
  \item{ylab}{Y-axis (row side) name. Character string. Can be missing
    or \code{NA}}
  \item{width}{Width suggested by the user. Can be \code{NA}}
  \item{height}{Height suggested by the user. Can be \code{NA}}
}
\details{
  \code{guessWH} determines for visual purposes the best height/width
  and legend/figure proportion for heatmaps. Interested users are
  invited to read the codes to get insights how the task is done.
}
\value{
  A list of width proportions, height proportions, the total width and
  the total length.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{biosHeatmap}}
}
\examples{
myMat <- matrix(rnorm(256), nrow=16)
rownames(myMat) <- sample(paste(letters, LETTERS, sep="_"),16)
colnames(myMat) <- sample(paste(LETTERS, letters, sep="_"), 16)
guessWH(nrow=nrow(myMat), ncol=ncol(myMat), width=4, height=4)
guessWH(nrow=nrow(myMat), ncol=ncol(myMat), width=NA, height=NA)
myWH <- guessWH(nrow=nrow(myMat), ncol=ncol(myMat),
xlab="321", ylab="ABC",
rownames=rownames(myMat), colnames=colnames(myMat))

if(interactive()) {
X11(width=myWH$width, height=myWH$height)
biosHeatmap(myMat, lwid=myWH$lwid, lhei=myWH$lhei, xlab="321",
ylab="ABC", cexRow=2L, cexCol=2L)
dev.off()
}

}
