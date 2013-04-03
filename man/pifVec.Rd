\name{pifVec}
\alias{pifVec}
\alias{rif1Vec}
\alias{rif2Vec}
\alias{pifMat}
\alias{rif1Mat}
\alias{rif2Mat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Vector- and matrix-version of PIF/RIF calculation
}
\description{
  Calculate PIF/RIF scores
}
\usage{
pifVec(x, y)
pifMat(x, y)

rif1Vec(xi, yi, xj, yj, method=c("spearman", "pearson", "kendall"))
rif1Mat(xReg, yReg, xDe, yDe, method=c("spearman", "pearson",
"kendall"))
rif2Vec(xi, yi, xj, yj, method=c("spearman", "pearson", "kendall"))
rif2Mat(xReg, yReg, xDe, yDe, method=c("spearman", "pearson",
"kendall"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A numeric vector or a matrix}
  \item{y}{A numeric vector or a matrix}
  
  \item{xi}{A numeric vector, expression of gene \code{i} in
    condition \code{x}}
  \item{yi}{A numeric vector, expression of gene \code{i} in
    condition \code{y}}
  \item{xj}{A numeric vector, expression of gene \code{j} in
    condition \code{x}}
  \item{yj}{A numeric vector, expression of gene \code{j} in
    condition \code{y}}

  \item{xReg}{A numeric matrix, rows represent expression of regulatory
    genes (e.g. TFs) in condition \code{x}}
  \item{yReg}{A numeric matrix, rows represent expression of regulatory
    genes in condition \code{y}}
  \item{xDe}{A numeric vector, rows represent expression of
    differentially regulated genes in condition \code{x}}
  \item{yDe}{A numeric vector, rows represent expression of
    differentially regulated genes in condition \code{y}}

  \item{method}{Methods to calculate correlation coefficients}
}
\details{
  These functions are low-level implementations of the PIF/RIF
  algorithm. You should rather consider using \code{RIFscore} unless you
  are sure what you are doing.
}
\value{
  PIF/RIF scores
}
\references{
  Hudson, Dalrymple, and Reverter. Beyond differential expression: the
  quest for causal mutations and effector molecules. BMC Genomics, 2012
  (13), 356.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{RIFscore}
}
\examples{
regN <- 50
deN <- 125
samX <- 8
samY <- 8

ERR <- 1E-8
set.seed(100)
reg1 <- matrix(rnorm(regN*samX), nrow=regN, byrow=TRUE)
reg2 <- matrix(rnorm(regN*samY)+1, nrow=regN, byrow=TRUE)
de1 <- matrix(rnorm(deN*samX), nrow=deN, byrow=TRUE)
de2 <- matrix(rnorm(deN*samY)+0.5, nrow=deN, byrow=TRUE)

## matrix-based method
system.time(rif1Res <- rif1Mat(reg1, reg2, de1, de2))
## vector-based method
system.time(rif1VecRes <- sapply(1:nrow(reg1), function(i)
                                 mean(sapply(1:nrow(de1), function(j)
                                             rif1Vec(reg1[i,], reg2[i,],
                                                     de1[j,], de2[j,])))))

stopifnot(all(abs(rif1Res-rif1VecRes)<ERR))
system.time(rif2Res <- rif2Mat(reg1, reg2, de1, de2))
system.time(rif2VecRes <- sapply(1:nrow(reg1), function(i)
                                 mean(sapply(1:nrow(de1), function(j)
                                             rif2Vec(reg1[i,], reg2[i,],
                                                     de1[j,], de2[j,])))))

stopifnot(all(abs(rif2Res-rif2VecRes)<ERR))

}

