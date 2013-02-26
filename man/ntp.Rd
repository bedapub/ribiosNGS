\name{ntp}
\alias{ntp}
\title{Nearest Template Prediction Algorithm}
\description{
  This function implements the Nearest Template Prediction (NTP)
  algorithm, a neareast-neighbor prediction approach (Hoshida, 2010). 
}
\usage{
ntp(matrix, genesets.ind, row.scale = TRUE, Nrand = 1000)
}
\arguments{
  \item{matrix}{Expression matrix}
  \item{genesets.ind}{A list of indices, representing gene sets
    (templates) to be tested. It can be derived by the \code{matchGenes}
    function.}
  \item{row.scale}{Logical, whether the expression matrix should be
    row-normalized. Unless gene signals in the same sample can be
    compared to each other directly, this option should be set to \code{TRUE}.}
  \item{Nrand}{Number of permutations to determine empirical p-values of
  prediction. By default 1000.}
}
\details{
  For algorithmic details see Hoshida, Plos ONE (2010). In GenePattern
  an implementation was given; however it has turned out to be very slow
  with even data of moderate size.

  This function implements the algorithm in C++ whenever possible. It
  uses matrix manipulations and parallel computing to accelerate the
  computation. Benchmark suggests that the current implementation can
  handle 40 samples and 150 templates within 40 seconds, about 40x
  faster than the naive implementation.

  Currently only cosine distance is supported as the the distance measure.
}
\value{
  A list of four items
  \item{prediction }{The predicted class label of each sample}
  \item{predPval }{Empirical p-value associated with the predictions}
  \item{distMat}{A matrix of distances between expression profiles and
    templates}
  \item{distMat}{A matrix of empirical p-values, derived from the
    comparison between distances and null distribution}
}
\references{
  Hoshida (2010) Nearest Template Prediction: A Single-Sample-Based
  Flexible Class Prediction with Confidence Assessment. PLoS ONE (2010)
  5:e15543. 
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\note{
  The number of cores used by the procedure can be controlled by
  \code{option(cores=N)}, where \code{N} is the desired number of cores
  to parallel the computation. It should be set before the function executes.
}
\examples{
testExp <- matrix(rnorm(1000), nrow=100)
rownames(testExp) <- paste("Gene", 1:nrow(testExp), sep="")
testExp[1:25, 1:5] <- testExp[1:25, 1:5] + rnorm(125, mean=1.5)
testExp[76:100, 6:10] <- testExp[76:100, 6:10] + rnorm(125, mean=1.5)

testGeneSetInd <- list(GeneSet1=1:25, GeneSet2=76:100)

#### Alternatively
## testGeneSet <- list(list(name="GeneSet1", description="",
##                     genes=rownames(testExp)[1:25]),
##                     list(name="GeneSet2", description="", genes=rownames(testExp)[76:100]))
## class(testGeneSet) <- "GeneSets"
## testGeneSetInd <- matchGenes(testGeneSet, rownames(testExp))

testNtpRes <- ntp(testExp, testGeneSetInd, row.scale=TRUE, Nrand=1000)
}

