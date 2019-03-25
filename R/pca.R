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
#' 
#' @examples 
#' myTestExprs <- matrix(rnorm(1000), ncol=10, byrow=FALSE)
#' myTestExprs[1:100, 6:10] <- myTestExprs[1:100, 6:10] + 2
#' myTopPca <- prcompExprs(myTestExprs, ntop=100)
prcompExprs <- function(matrix, ntop=NULL) {
  if(!is.null(ntop) && !is.na(ntop)) {
    rv <- rowVars(matrix)
    select <- order(rv, decreasing = TRUE)[seq_len(min(ntop, 
                                                       length(rv)))]
    matrix <- matrix[select,]
  }
  tMatAll <- t(matrix)
  isInvar <- rowVars(tMatAll) == 0
  tMat <- tMatAll[!isInvar,, drop=FALSE]
  res <- prcomp(tMat, center=TRUE, scale.=TRUE) 
  return(res)
}

#' Principal component analysis of DGEList
#' 
#' @param x A \code{DGEList} object
#' @param ntop Integer, how many top-variable genes should be used?
#' @param fun Function, how to transform counts in the DGEList into data appropriate for PCA? log-cpm is used by default.
#' 
#' If many genes have zero count in all samples, the PCA plot of samples can be sometimes delusive. Therefore, the function
#' removes such all-zero-count features prior to PCA analysis.
#' 
#' @examples
#' myCounts <- matrix(rnbinom(100, 3, 0.25), nrow=10)
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
                           fun=function(x) cpm(x, log=TRUE)) {
  ## remove all-zero-count features first, otherwise the PCA result can be delusive
  x <- x[rowSums(x$counts)>0, 1:ncol(x)]
  mat <- do.call(fun, list(x))
  res <- prcompExprs(mat, ntop=ntop)
  return(res)
}
