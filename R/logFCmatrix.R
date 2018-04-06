#' Extract a matrix of log2(fold-change) values
#' 
#' @param edgeResult An \code{EdgeResult} object
#' @param featureIdentifier Character, column name in \code{dgeTable} that will be used as rownames of the result matrix
#' @param contrast \code{NULL} or characters; if not \code{NULL}, only logFC values of given contrasts will be returned
#' 
#' @note TODO: add edgeResult data example
logFCmatrix <- function(edgeResult, featureIdentifier="GeneSymbol", contrasts=NULL) {
  tbls <- edgeResult@dgeTables
  allContrasts <- contrastNames(edgeResult)
  
  stopifnot(featureIdentifier %in% colnames(tbls[[1]]))
  oriFeats <- rownames(tbls[[1]])
  feats <- tbls[[1]][, featureIdentifier]
  mat <- sapply(tbls, function(x) matchColumn(oriFeats, x, 0L)$logFC)
  rownames(mat) <- feats
  colnames(mat) <- allContrasts
  if(!is.null(contrasts)) {
    if (is.logical(contrasts) || is.numeric(contrasts)) {
      contrasts <- allContrasts[contrasts]
    }
    mat <- mat[, contrasts]
  }
  return(mat)
}
