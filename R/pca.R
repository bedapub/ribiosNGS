#' Variance of features in rows
#' 
#' 
#' @param x Numeric matrix
#' @param na.rm Logical. Should missing values (including NaN) be omitted from
#' the calculations?
#' @examples
#' 
#' myVal <- matrix(1:9, nrow=3, byrow=FALSE)
#' myVar <- rowVars(myVal)
#' stopifnot(identical(myVar, c(9,9,9)))
#' 
#' @export rowVars
rowVars <- function (x, na.rm=TRUE) {
  sqr = function(x) x * x
  n = rowSums(!is.na(x))
  n[n <= 1] = NA
  return(rowSums(sqr(x - rowMeans(x, na.rm=na.rm)), na.rm=na.rm)/(n - 1))
}


#' Bin rows by mean expression, and return integer indices of rows with the highest variations from each bin
#' 
#' @param matrix A numeric matrix
#' @param ntop Integer, the total number of rows that are expected to be returned
#' @param nbin Integer, how many bins should be formed? If \code{NULL}, an automatic value is used.
#' @return An integer vector of rows, containing indices
#' 
#' The function first bin rows by their average expression profile, and choose rows with the highest variation from each bin.
#' 
#' @note In case \code{ntop} is not a multiple of \code{nbin}, the \code{modulo} genes with the loweset variation are removed to garantee that the resulting matrix has exactly \code{ntop} rows.
#' 
#' @examples
#' # myMat <- matrix(rnorm(2000), ncol=10, byrow=FALSE)
#' # myTopVarMatInd <- topVarRowIndByMeanBinning(myMat, ntop=130, nbin=15)
#' # myTopVarMatInd2 <- topVarRowIndByMeanBinning(myMat, ntop=135, nbin=15)
#' @keywords internal
topVarRowIndByMeanBinning <- function(matrix, ntop=NULL, nbin=NULL) {
  if(is.null(ntop) || is.na(ntop)) {
    return(matrix)
  } else if (is.integer(ntop) && ntop>nrow(matrix)) {
    return(matrix)
  } 
  
  if(is.null(nbin) || is.na(nbin)) {
    nbin <- pmin(100,
                 pmax(nrow(matrix) %/% 10, 1L))
  }

  rv <- rowVars(matrix)
  rm <- rowMeans(matrix, na.rm=TRUE)
  rmf <- cut(rank(rm), breaks=nbin)
  countEachBin <- ceiling(ntop/nbin)
  selects <- tapply(1:nrow(matrix), rmf, function(ind) {
    binrv <- rv[ind]
    selLen <- pmin(countEachBin,length(ind))
    selInd <- seq_len(selLen)
    ord <- order(binrv, decreasing=TRUE)[selInd]
    res <- ind[ord]
  })
  select <- unname(unlist(selects))
  if(ntop %% nbin!=0) {
    matrix <- matrix[select,, drop=FALSE]
    rv <- rowVars(matrix)
    select <- order(rv, decreasing=TRUE)[seq_len(ntop)]
  }
  return(select)
}

#' Bin rows by mean expression, and return rows with the highest variations from each bin
#' 
#' @param matrix A numeric matrix
#' @param ntop Integer, the total number of rows that are expected to be returned
#' @param nbin Integer, how many bins should be formed? If \code{NULL}, an automatic value is used.
#' @return A subset of the matrix, containing exactly \code{ntop} rows.
#' 
#' @seealso \code{\link{topVarRowIndByMeanBinning}}
#' @examples
#' # myMat <- matrix(rnorm(2000), ncol=10, byrow=FALSE)
#' # myTopVarMat <- topVarRowsByMeanBinning(myMat, ntop=130, nbin=15)
#' # myTopVarMat2 <- topVarRowsByMeanBinning(myMat, ntop=135, nbin=15)
#' @keywords internal
topVarRowsByMeanBinning <- function(matrix, ntop=NULL, nbin=NULL) {
  inds <- topVarRowIndByMeanBinning(matrix, ntop=ntop, nbin=nbin)
  res <- matrix[inds,, drop=FALSE]
  return(res)
}

#' Principal component analysis of an expression matrix
#' 
#' 
#' @param matrix Numeric matrix. Features in rows and samples in columns.
#' @param ntop Integer or NULL. If not \code{NULL}, only \code{ntop} genes with
#' the highest variance are used for the calculation.
#' @param scale Logical, whether variance of features should be scaled to 1.
#' Default \code{FALSE}, as recommended by Nguyen et al. (2019)
#' @param nbin Integer. Genes are divided into \code{nbin} bins by their average gene expression signal, and top variable genes (approximately \code{ntop/nbin}) are selected from each bin. If \code{NULL} or \code{NA}, an automatic value (100, or \code{nrow(matrix) \%/\% 10} when fewer are 1000 genes are used as input) is used. It is only used when \code{ntop} is not NULL.
#' 
#' @references Nguyen, Lan Huong, and Susan Holmes. "Ten Quick Tips for
#' Effective Dimensionality Reduction." PLOS Computational Biology 15, no. 6
#' (2019): e1006907
#' 
#' @seealso \code{\link{topVarRowsByMeanBinning}}
#' @examples
#' 
#' myTestExprs <- matrix(rnorm(1000), ncol=10, byrow=FALSE)
#' myTestExprs[1:50, 6:10] <- myTestExprs[1:50, 6:10] + 2
#' myTopPca <- prcompExprs(myTestExprs, ntop=50, nbin=5)
#' 
#' @export prcompExprs
prcompExprs <- function(matrix, ntop=NULL, scale=FALSE, nbin=NULL) {
  if(!is.null(ntop) && !is.na(ntop)) {
    matrix <- topVarRowsByMeanBinning(matrix, ntop=ntop, nbin=nbin)
  }
  tMatAll <- t(matrix)
  isInvar <- rowVars(tMatAll) == 0
  tMat <- tMatAll[!isInvar,, drop=FALSE]
  res <- prcomp(tMat, center=TRUE, scale.=scale) 
  return(res)
}

#' Principal component analysis of DGEList
#' 
#' @param x A \code{DGEList} object
#' @param ntop Integer, how many top-variable features should be used? If \code{NULL}, all features are used
#' @param scale Logical, whether variance of features should be scaled to 1. \code{FALSE} by default (recommended!); set it to \code{TRUE} only if you are sure what you are doing
#' @param verbose Logical, whether the function should print messages.
#' @param ... Other parameters passed to \code{\link[vsn]{vsnMatrix}}
#' 
#' The function first remove all-zero-count features, because they can make the PCA plot of samples delusive. 
#' 
#' Next, it applies \code{vsn} transformation implemented in the \code{vsn} package to the count matrix. 
#' 
#' Finally, PCA is applied to the vsn-transformed matrix. 
#' 
#' @return The function returns a \code{prcomp} object. The fit object is saved in the \code{vsnFit} field in the returned object, and the transformed matrix is saved in the \code{vsnMat} field.
#' 
#' @seealso \code{\link{prcompExprs}}
#' 
#' @examples
#' 
#' myCounts <- matrix(rnbinom(1000, 3, 0.25), nrow=100)
#' myDgeList <- DGEList(counts=myCounts,
#'   samples=data.frame(group=gl(5,2)))
#' myPrcomp <- prcomp(myDgeList)
#' 
#' \dontrun{
#'   vsn::meanSdPlot(myPrcomp$vsnFit)
#' }
#' 
#' ## features with zero count in all samples do not contribute to the PCA analysis
#' myDgeList2 <- DGEList(counts=rbind(myCounts, rep(0, 10)),
#'   samples=data.frame(group=gl(5,2)))
#' myPrcomp2 <- prcomp(myDgeList2)
#' stopifnot(identical(myPrcomp, myPrcomp2))
#' 
#' @export prcomp.DGEList
prcomp.DGEList <- function(x, ntop=NULL, 
                           scale=FALSE,
                           verbose=FALSE,
                           ...) {
  ## remove all-zero-count features first, otherwise the PCA result can be delusive
  x <- x[rowSums(x$counts)>0, 1:ncol(x)]
  fit <- vsn::vsnMatrix(x$counts, verbose=verbose, ...)
  mat <- vsn::predict(fit, newdata=x$counts, useDataInFit=TRUE)
  res <- prcompExprs(mat, ntop=ntop, scale=scale)
  res$vsnFit <- fit
  res$vsnMat <- mat
  return(res)
}
