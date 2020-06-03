#' @include logFCmatrix.R
NULL

#' Perform principal component analysis to the log fold-change matrix
#' @param edgeResult An \code{EdgeResult} object
#' @return A \code{PCAScoreMatrix} object
#' 
#' The function performs principal component analysis (PCA) to the log 
#' fold-change matrix. 
#' 
#' By using a column of zeros during the PCA analysis,
#' which was removed from the final result, the point of origin represents 
#' an ideal contrast which yield absolutely no differential gene expression. 
#' It is easier to interpret the PCA results with this transformation.
#' 
#' @importFrom stats prcomp
#' @importFrom ribiosPlot pcaScores
#' @export
#' @examples 
#' ## TODO: add edgeResult sample
logFCpca <- function(edgeResult) {
  lfc <- logFCmatrix(edgeResult)
  lfcWithZero <- cbind(lfc, ZeroLfc=0)
  lfcPca <- stats::prcomp(t(lfcWithZero))
  scores <- pcaScores(lfcPca, offset=ncol(lfcWithZero))
  scores <- scores[-nrow(scores),,drop=FALSE]
  return(scores)
}
