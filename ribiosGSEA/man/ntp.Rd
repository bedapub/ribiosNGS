\name{ntp}
\alias{ntp}
\alias{ntpTemplates}
\alias{ntpBiTemplates}
\title{Nearest Template Prediction Algorithm}
\description{
  This function implements the Nearest Template Prediction (NTP)
  algorithm, a neareast-neighbor prediction approach (Hoshida, 2010). 
}
\usage{
ntpTemplates(genesets, featureNames)
ntpBiTemplates(genesetsPos, genesetsNeg, featureNames)
ntp(matrix, ntpTemplates, row.scale = TRUE, Nrand = 1000)
}
\arguments{
  \item{genesets}{A GeneSets S3-object, probably returned by
    \code{readGmt}}
  \item{genesetsPos}{A GeneSets S3-object including positive markers,
    probably returned by \code{readGmt}}
  \item{genesetsNeg}{A GeneSets S3-object including negative markers,
    probably returned by \code{readGmt}}
  \item{featureNames}{Feature names of the expression dataset to be
    matched by gene identifiers in GeneSets.}
  
  \item{matrix}{Expression matrix}
  \item{ntpTemplates}{A list of indices, representing gene sets
    (templates) to be tested. It can be derived by the \code{matchGenes}
    function.}
  \item{row.scale}{Logical, whether the expression matrix should be
    row-normalized. Unless gene signals in the same sample can be
    compared to each other directly, this option should be set to \code{TRUE}.}
  \item{Nrand}{Number of permutations to determine empirical p-values of
    prediction. By default 1000.}
}
\details{
  The algorithm runs in two steps. First a set of templates must be
  built (encapsulated in an \code{ntpTemplates} object). The starting
  point is one (for \code{ntpTemplates}) or two (\code{ntpBiTemplates})
  GeneSets objects, which probably are returned by the \code{readGmt}
  function. Templates built by \code{ntpTemplates} implements the
  original algorithm by Hoshida, while \code{ntpBiTemplates} generates
  templates that can include negative genes in cases of multiple (n>=3)
  groups.

  In the second step predictions are done by comparing signature gene
  expression profiles of the expression matrix to the templates.
}
\value{
  \code{ntpTemplates} or \code{ntpBiTemplates} returns a
  \code{ntpTemplates} object.
  
  \code{ntp} returns a list of four items:
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
  For algorithmic details see Hoshida, Plos ONE (2010). In GenePattern
  an implementation was given; however it has turned out to be very slow
  with even data of moderate size.

  This function implements the algorithm in C++ whenever possible. It
  uses matrix manipulations and parallel computing to accelerate the
  computation. Benchmark suggests that the current implementation can
  handle 40 samples and 150 templates within 40 seconds, about 40x
  faster than the naive implementation.

  Cosine distance is used as the distance measure.

  The number of cores used by the procedure can be controlled by
  \code{option(cores=N)}, where \code{N} is the desired number of cores
  to parallel the computation (Default: 6). It should be set before the
  function executes. 
}
\examples{
testExp <- matrix(rnorm(1000), nrow=100)
rownames(testExp) <- paste("Gene", 1:nrow(testExp), sep="")
testExp[1:25, 1:5] <- testExp[1:25, 1:5] + rnorm(125, mean=1.5)
testExp[76:100, 6:10] <- testExp[76:100, 6:10] + rnorm(125, mean=1.5)

gs1 <- GeneSet(name="GeneSet1", genes=rownames(testExp)[1:25])
gs2 <- GeneSet(name="GeneSet1", genes=rownames(testExp)[76:100])
testGeneSet <- GeneSets(gs1, gs2)

## First step: building templates
testTemplates <- ntpTemplates(testGeneSet, rownames(testExp))

## Second step: running the NTP algorithm
testNtpRes <- ntp(testExp, testTemplates, row.scale=TRUE, Nrand=1000)


## test ntpBiTemplates
posGmtFile <- system.file("extdata/gmts/posGS.gmt",
package="ribiosGSEA")
negGmtFile <- system.file("extdata/gmts/negGS.gmt",
package="ribiosGSEA")
testPos <- readGmt(posGmtFile)
testNeg <- readGmt(negGmtFile)
testBiTemp <- ntpBiTemplates(testPos, testNeg, LETTERS[1:8])
}

