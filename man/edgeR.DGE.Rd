\name{edgeR.DGE}
\alias{edgeR.DGE}
\title{Read and pre-process digital gene expression data with edgeR functions}
\description{
  The function is a shortcut to common read-in and pre-processing
  operations applied to DGE (digital gene expression data), with
  functions from the \code{edgeR} function.
}
\usage{
edgeR.DGE(path = ".", targets.file = "Targets.txt", normFactor.method = "RLE")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{Character, file path containing DGE files}
  \item{targets.file}{Character, name of a file in the \code{path}
    specified, containing targets information.}
  \item{normFactor.method}{Method to estimate the normalization factors}
}
\value{A \code{DEGList} object, with normalization factors and estimated
common dispersion}
\author{
  Jitao David Zhang and Rolland Schmucki
}
\seealso{
  \code{readDGE}, \code{calcNormFactors} and \code{estimateCommonDist}
  functions in the \code{edgeR} package.
}
\examples{
mypath <- system.file("extdata/sage", package="ribiosNGS")
mytarget <- "Targets25.txt"
myd <- edgeR.DGE(path=mypath, targets.file=mytarget)
}
