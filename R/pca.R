#' Variance of features in rows
#' 
#' @param x Numeric matrix
#' @param na.rm Logical. Should missing values (including NaN) be omitted from the calculations?
#'
#' @examples
#' myVal <- matrix(1:9, nrow=3, byrow=FALSE)
#' myVar <- rowVars(myVal)
#' stopifnot(identical(myVar, c(9,9,9)))
rowVars <- function (x, na.rm=TRUE) {
  sqr = function(x) x * x
  n = rowSums(!is.na(x))
  n[n <= 1] = NA
  return(rowSums(sqr(x - rowMeans(x, na.rm=na.rm)), na.rm=na.rm)/(n - 1))
}

#' Principal component analysis of expression matrix
#' 
#' @param matrix Numeric matrix. Features in rows and samples in columns.
#' @param ntop Integer or NULL. If not \code{NULL}, only \code{ntop} genes with the highest variance are used for the calculation.
#' @param scale Logical, whether variance of features should be scaled to 1. Default \code{FALSE}, as recommended by Nguyen et al. (2019)
#' @references Nguyen, Lan Huong, and Susan Holmes. "Ten Quick Tips for Effective Dimensionality Reduction." PLOS Computational Biology 15, no. 6 (2019): e1006907
#' @examples 
#' myTestExprs <- matrix(rnorm(1000), ncol=10, byrow=FALSE)
#' myTestExprs[1:100, 6:10] <- myTestExprs[1:100, 6:10] + 2
#' myTopPca <- prcompExprs(myTestExprs, ntop=100)
prcompExprs <- function(matrix, ntop=NULL, scale=FALSE) {
  if(!is.null(ntop) && !is.na(ntop)) {
    rv <- rowVars(matrix)
    select <- order(rv, decreasing = TRUE)[seq_len(min(ntop, 
                                                       length(rv)))]
    matrix <- matrix[select,]
  }
  tMatAll <- t(matrix)
  isInvar <- rowVars(tMatAll) == 0
  tMat <- tMatAll[!isInvar,, drop=FALSE]
  res <- prcomp(tMat, center=TRUE, scale.=scale) 
  return(res)
}

#' Fit the vsn model to log2CPM data
#' @param x A matrix or DGEList object
#' @param prior.count Integer, passed to \code{\link[edgeR]{cpm}}
#' @param normalized.lib.sizes Logical, passed to \code{\link[edgeR]{cpm}}
#' @param verbose Logical, whether diagnostic information is printed for vsn
#' @param ... Other parameters passed to \code{\link[vsn]{justvsn}}
#' 
#' @return A matrix of vsn-transformed expression data
#' @examples 
#' myCounts <- matrix(rnbinom(1000, 10, 0.5), nrow=100)
#' myDgeList <- DGEList(counts=myCounts,
#'   samples=data.frame(group=gl(5,2)))
#'   
#' myCpmVsn2MatRes <- cpmVsn2(myCounts)
#' myCpmVsn2DGEListRes <- cpmVsn2(myDgeList)
cpmVsn2 <- function(x, prior.count=2, normalized.lib.sizes=TRUE,
                   verbose=FALSE, ...) {
  cpmRes <- edgeR::cpm(x, log=TRUE, prior.count=prior.count,
                normalized.lib.sizes = normalized.lib.sizes)
  res <- vsn::justvsn(cpmRes, verbose=verbose, ...)
  return(res)
}

#' Principal component analysis of DGEList
#' 
#' @param x A \code{DGEList} object
#' @param ntop Integer, how many top-variable genes should be used?
#' @param fun Function, how to transform counts in the DGEList into data appropriate for PCA? vsn2 transformation of log2-cpm is used by default.
#' @param scale Logical, whether variance of features should be scaled to 1. Default \code{FALSE}
#' 
#' If many genes have zero count in all samples, the PCA plot of samples can be sometimes delusive. Therefore, the function
#' removes such all-zero-count features prior to PCA analysis.
#' 
#' @seealso \code{\link{prcompExprs}}
#' 
#' @examples
#' myCounts <- matrix(rnbinom(1000, 3, 0.25), nrow=100)
#' myDgeList <- DGEList(counts=myCounts,
#'   samples=data.frame(group=gl(5,2)))
#' myPrcomp <- prcomp(myDgeList)
#' 
#' #' features with zero count in all samples do not contribute to the PCA analysis
#' myDgeList2 <- DGEList(counts=rbind(myCounts, rep(0, 10)),
#'   samples=data.frame(group=gl(5,2)))
#' myPrcomp2 <- prcomp(myDgeList2)
#' stopifnot(identical(myPrcomp, myPrcomp2))
prcomp.DGEList <- function(x, ntop=NULL, 
                           scale=FALSE,
                           fun=function(x) cpmVsn2(x)) {
  ## remove all-zero-count features first, otherwise the PCA result can be delusive
  x <- x[rowSums(x$counts)>0, 1:ncol(x)]
  mat <- do.call(fun, list(x))
  res <- prcompExprs(mat, ntop=ntop, scale=scale)
  return(res)
}
