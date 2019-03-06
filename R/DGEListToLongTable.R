#' Convert a DGEList object to a long data.frame containing expression, feature annotation, and sample annotation
#' 
#' @param x A DGEList object
#' @param exprsFun The function to convert counts to expression data. Default: logCPM
#' 
#' @examples
#' mat <- matrix(rnbinom(100, mu=5, size=2), ncol=10)
#' rownames(mat) <- sprintf("gene%d", 1:nrow(mat))
#' y <- edgeR::DGEList(counts=mat, group=rep(1:2, each=5))
#' DGEListToLongTable(y)

DGEListToLongTable <- function (x, 
                                exprsFun = function(dgeList) cpm(dgeList, log=TRUE)) {
  exp <- do.call(exprsFun, list(x))
  if (is.data.frame(exp)) {
    expVec <- unlist(exp)
    rownames(expVec) <- rownames(exp)
    exprsLong <- as.data.frame(expVec)
  } else {
    exprsLong <- as.data.frame(as.vector(exp))
  }
  colnames(exprsLong) <- "exprs"
  fDataCol <- colnames(x$genes)
  pDataCol <- colnames(x$samples)
  pfCommon <- intersect(fDataCol, pDataCol)
  for (i in colnames(fData(x))) {
    if (i %in% pfCommon) {
      inew <- sprintf("fData.%s", i)
    }
    else {
      inew <- i
    }
    exprsLong[, inew] <- rep(fData(x)[, i], dim(x)[2])
  }
  for (j in colnames(pData(x))) {
    if (j %in% pfCommon) {
      jnew <- sprintf("pData.%s", j)
    }
    else {
      jnew <- j
    }
    exprsLong[, jnew] <- rep(pData(x)[, j], each = dim(x)[1])
  }
  return(exprsLong)
}
