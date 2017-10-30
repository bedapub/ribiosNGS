## transformations
matrixToLongTable <- function(x, valueLabel="value", rowLabel="row", colLabel="col") {
  val <- as.vector(x)
  rowLabels <- rep(rownames(x), ncol(x))
  colLabels <- rep(colnames(x), each=nrow(x))
  res <- data.frame(rowLabl=rowLabels,
                    colLabels=colLabels,
                    value=val)
  colnames(res) <- c(rowLabel, colLabel, valueLabel)
  return(res)
}

#' Transform eSet to long data.frame
#' 
#' @param x An \code{eSet} object
#' @param exprsFun A function to extract expression values, by default \code{exprs}
#' 
#' The function extracts gene expression, and return it in a long data.frame format with phenotypic data
#' 
#' @examples 
#' data(ribios.ExpressionSet, package="ribiosExpression")
#' exprsLongTbl <- eSetToLongTable(ribios.ExpressionSet)
#' seLongTbl <- eSetToLongTable(ribios.ExpressionSet, exprsFun=function(eset) assayData(eset)$se.exprs)
eSetToLongTable <- function(x, exprsFun=function(eset) Biobase::exprs(eset)) {
  exp <- do.call(exprsFun, list(x))
  if(is.data.frame(exp)) {
    expVec <- unlist(exp)
    rownames(expVec) <- rownames(exp)
    exprsLong <- as.data.frame(expVec)
  } else {
    exprsLong <- as.data.frame(as.vector(exp))    
  }
  
  colnames(exprsLong) <- "exprs"
  fDataCol <- colnames(fData(x))
  pDataCol <- colnames(pData(x))
  pfCommon <- intersect(fDataCol, pDataCol)
  
  for(i in colnames(fData(x))) {
    if(i %in% pfCommon) {
      inew <- sprintf("fData.%s", i)
    } else {
      inew <- i
    }
    exprsLong[, inew] <- rep(fData(x)[,i], dim(x)[2])
  }

  for(j in colnames(pData(x))) {
    if(j %in% pfCommon) {
      jnew <- sprintf("pData.%s", j)
    } else {
      jnew <- j
    }
    exprsLong[, jnew] <- rep(pData(x)[,j], each=dim(x)[1])
  }
  return(exprsLong)
}

