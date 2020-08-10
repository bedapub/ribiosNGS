utils::globalVariables("AveExpr")

#' Extract a matrix of log2(fold-change) values
#' 
#' @param edgeResult An \code{EdgeResult} object
#' @param featureIdentifier Character, column name in \code{dgeTable} that will
#' be used as rownames of the result matrix
#' @param contrasts \code{NULL} or characters; if not \code{NULL}, only logFC
#' values of given contrasts will be returned
#' @param minAveExpr \code{NULL} or numeric. If set, features with aveExpr lower
#' than the given value is not considered. This option is helpful to remove
#' genes that are lowly expressed which yet show strong differential
#' expression.
#' @param removeNAfeatures Logical, if \code{TRUE}, features containing \code{NA}
#'   values are removed.
#' @note TODO: add edgeResult data example
#' @importFrom ribiosUtils mintersect
#' @export logFCmatrix
logFCmatrix <- function(edgeResult, featureIdentifier="GeneSymbol", 
                        contrasts=NULL,
                        removeNAfeatures=TRUE,
                        minAveExpr=NULL) {
  tbls <- edgeResult@dgeTables
  if(!is.null(minAveExpr)) {
    tbls <- lapply(tbls, function(x) subset(x, AveExpr>=minAveExpr))
  }
  allContrasts <- contrastNames(edgeResult)
  
  stopifnot(featureIdentifier %in% colnames(tbls[[1]]))
  oriFeats <- ribiosUtils::mintersect(lapply(tbls, rownames))
  feats <- tbls[[1]][, featureIdentifier]
  mat <- sapply(tbls, function(x) ribiosUtils::matchColumn(oriFeats, x, 0L)$logFC)
  rownames(mat) <- feats
  colnames(mat) <- allContrasts
  if(removeNAfeatures) {
      isNAfeat <- is.na(feats) | feats=="-"
      mat <- mat[!isNAfeat,,drop=FALSE]
  }
  if(!is.null(contrasts)) {
    if (is.logical(contrasts) || is.numeric(contrasts)) {
      contrasts <- allContrasts[contrasts]
    }
    mat <- mat[, contrasts, drop=FALSE]
  }
  return(mat)
}
