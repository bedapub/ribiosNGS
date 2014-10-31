#' Build DGEList object ready for generalized linear model fitting
#'
#' @description
#' The function combines the step of object initialization, library size calculation, and dispersion estimation 
#'
#' @note
#' robust=TRUE is only valid for edgeR version >= 3.6.0
#'
#' @param x a numerical matrix or any object that can be transformed as such
#' @param design design matrix (subject to dimensionality check)
#' @param robust Whether robust estimation (Zhou \emph{et al.}, NAR 2014) should be used
#' @return A DGEList object with dispersions estimated, ready for \code{glmFit}.
#' @seealso \code{glmFit} to fit a generalized linear model
#' @examples
#' expression <- matrix(rnbinom(10000,mu=5,size=2),ncol=4)
#' design <- cbind(baseline=c(1,1,1,1), x=c(1,1,-1,-1))
#' edgeObj <- edgeBuild(expression, design)
edgeBuild <- function(x, design, robust=FALSE) {
  x <- as.matrix(x)
  dge <- DGEList(counts=x)
  dge <- calcNormFactors(dge, method="TMM")
  if(robust) {
    dge <- estimateGLMRobustDisp(dge, design)
  } else {
    dge <- estimateGLMCommonDisp(dge, design, verbose=FALSE)
    dge <- estimateGLMTrendedDisp(dge, design, verbose=FALSE)
    dge <- estimateGLMTagwiseDisp(dge, design)
  }
  return(dge)
}

#' Test differentially expressed genes using edgeR
#'
#' @description
#' The function tests differentially expressed genes using generalized linear model likelihood-ratio tests
#' A single contrast vector is allowed
#'
#' @param fit An \code{DGEGLM} object produced by \code{glmFit}
#' @param contrast a vector of contrasts
#' @return A data frame containing information about differential expression
#' @seealso \code{\link{glmFit}} to produce the input object, \code{\link{edgeTest}} to run the test for a contrast matrix.
#' 
#' @examples
#' expression <- matrix(rnbinom(10000,mu=5,size=2),ncol=4)
#' design <- cbind(baseline=c(1,1,1,1), x=c(1,1,-1,-1))
#' edgeObj <- edgeBuild(expression, design)
#' edgeFit <- glmFit(edgeObj, design)
#' edgeFitTest <- edgeTestContrast(edgeFit, contrast=c(0,1))
edgeTestContrast <- function(fit, contrast) {
  stopifnot(is.vector(contrast))
  lrt <- glmLRT(fit, contrast=contrast)
  x <- topTags(lrt, n=nrow(lrt$table))$table
  return(x)
}

#' Test differentially expressed genes using edgeR
#'
#' @description
#' The function tests differentially expressed genes using generalized linear model likelihood-ratio tests
#' Contrasts in the matrix are sequentially fitted
#'
#' @param fit An \code{DGEGLM} object produced by \code{glmFit}
#' @param contrasts a contrast matrix. If a vector is given, it is converted into a column vector.
#' @return A list of data frames containing information about differential expression
#' @seealso \code{\link{edgeTestContrast}} to test one contrast vector
#' 
#' @examples
#' expression <- matrix(rnbinom(10000,mu=5,size=2),ncol=4)
#' design <- cbind(baseline=c(1,1,1,1), x=c(1,1,-1,-1), y=c(-1,1,-1,1))
#' edgeObj <- edgeBuild(expression, design)
#' edgeFit <- glmFit(edgeObj, design)
#' contrast1 <- c(0,1, 0)
#' contrast2 <- c(0,1, -1)
#' contrast3 <- cbind(C1=contrast1, C2=contrast2)
#' edgeTest1 <- edgeTest(edgeFit, contrast1)
#' edgeTest2 <- edgeTest(edgeFit, contrast2)
#' edgeTest3 <- edgeTest(edgeFit, contrast3)
#' stopifnot(identical(names(edgeTest3), c("C1", "C2")))
edgeTest <- function(fit,  contrasts) {
  if(is.vector(contrasts)) contrasts <- cbind(contrasts)
  assertContrast(fit$design, contrasts)
  toptables <- apply(contrasts, 2, function(x)
                    edgeTestContrast(fit, x))
  return(toptables)
}

#' Identify differentially expressed genes using generalized linear models implemented by edgeR
#'
#' @description
#' The function provides a simple interface to run differential gene analysis using edgeR using default settings
#' Users desiring higher flexibility may call functions individually
#' It is user's responsibility to understand the assumptions behind the default settings and to judge whether they are suitable for the data
#' @param x A matrix of read counts, or any objects that can be converted to a matrix
#' @param design Design matrix (subject to dimensionality check)
#' @param contrasts Contrast matrix (subject to dimensionality check)
#' @param robust Logical, whether robust dispersion estimation should be used or not
#'
#' @return A list of data frames reporting differntial gene expression
#' @seealso
#' \code{\link{edgeBuild}} to build a DGEList object and to estimate dispersions
#' \code{\link{glmFit}} to fit a DGEList object against the design matrix
#' \code{\link{edgeTest}} to derive lists of differentially expressed genes
#'
#' #' @note
#' robust=TRUE is only valid for edgeR version >= 3.6.0
#'
#' @examples
#' expression <- matrix(rnbinom(10000,mu=5,size=2),ncol=4)
#' design <- cbind(baseline=c(1,1,1,1), x=c(1,1,-1,-1), y=c(-1,1,-1,1))
#' contrast <- cbind(C1=c(0,1,0), C2=c(0,1,-1))
#' edgeDiff <- edgeRun(expression, design, contrast, robust=FALSE)
edgeRun <- function(x, design, contrasts, robust=FALSE) {
  dge <- edgeBuild(x, design, robust=robust)
  fit <- glmFit(dge, design)
  test <- edgeTest(fit, contrasts)
}

## zhangj83 2014-10-31
## the following two functions are going to be factored out
##edgeR.DGE <- function(path=".",
##                      targets.file="Targets.txt",
##                      normFactor.method="RLE") {
##  target.file.full <- file.path(path,targets.file)
##  if(!file.exists(target.file.full))
##    stop("target.file not found at ", target.file.full)
##  targets <- read.delim(file=target.file.full,
##                        stringsAsFactors=FALSE)
##  data <- readDGE(targets, path=path, comment.char="#", head=FALSE)
##  d <- calcNormFactors(data, method=normFactor.method)
##  d <- estimateCommonDisp(d)
##  d
##}
##
##edgeR.sumCountByGroup <- function(data,
##                                  group.colName="group"){
##  if(!group.colName %in% colnames(data$sample))
##    stop("'group.colName' must be one of the following:\n",
##         paste(colnames(data$sample), collapse=","))
##  do.call(cbind,
##          tapply(1:ncol(data$counts),
##                 data$sample[,group.colName],
##                 function(x) rowSums(data$counts[,x,drop=FALSE])))
##}
