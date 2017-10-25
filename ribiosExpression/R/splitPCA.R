#' Split an eSet object and run PCA on each split, return PCA scores as one data.frame
#' 
#' @param eset An eSet object
#' @param factor One or more factor vectors, used to split the eSet object
#' @param func Function to retrieve values from split sub-eset objects
#' 
#' @examples 
#' data(ribios.ExpressionSet, package="ribiosExpression")
#' fac1 <- gl(2,13)
#' pcaScore1 <- splitPCA(ribios.ExpressionSet, fac1)
splitPCA <- function(eset, factor, func=function(e) exprs(e), ...) {
  resList <- tapply(1:ncol(eset), factor, function(i) {
    if(length(i)==0)
      return(NULL)
    if(length(i)==1) {
      warning(sprintf("Single sample in the group - no PCA is available for sample %d",
                      i))
      return(NULL)
    }
    subEset <- eset[, i]
    val <- do.call(func, list(subEset))
    pcaRes <- prcomp(t(val))
    res <- data.frame(ribiosPlot::pcaScores(pcaRes, ...),
                      pData(subEset))
    return(res)
  })
  res <- do.call(rbind, resList)
  return(res)
}
