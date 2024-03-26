#' @include logFCmatrix.R
NULL

#' Perform principal component analysis to the log fold-change matrix
#' @param lfc A matrix of log2 fold changes, with features in rows and
#'  contrasts in columns
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
#' my_lfc_mat <- matrix(rnorm(1000), nrow=100, ncol=10)
#' my_lfc_pca <- logFCmatrixPCA(my_lfc_mat)
#' my_lfc_pca
logFCmatrixPCA <- function(lfc) {
  lfcWithZero <- cbind(lfc, ZeroLfc = 0)
  lfcPca <- stats::prcomp(t(lfcWithZero))
  scores <- pcaScores(lfcPca, offset = ncol(lfcWithZero))
  scores <- scores[-nrow(scores), , drop = FALSE]
  return(scores)
}

#' Perform principal component analysis to an EdgeResult object
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
#' @seealso logFCmatrixPCA
#' @export
#' @examples 
#' ## TODO: add edgeResult sample
logFCpca <- function(edgeResult) {
  lfc <- logFCmatrix(edgeResult)
  scores <- logFCmatrixPCA(lfc)
  return(scores)
}
