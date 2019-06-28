#' Convert count matrix to TPM values
#' 
#' 
#' @param x A count matrix or other objects that can be converted to a matrix
#' by \code{as.matrix}
#' @param gene.length A numeric vector of the same length as the row count of
#' \code{x}, giving gene lengths
#' @return transcripts per million (TPM) values
#' @seealso \code{\link{rpkm2tpm}}
#' @examples
#' 
#' testMatrix <- matrix(rnbinom(200, size=5, prob=0.1), nrow=20, ncol=10)
#' testMatrixGeneLen <- as.integer(10^rnorm(20, mean=3, sd=0.5))
#' testMatrixTpm <- tpm(testMatrix, testMatrixGeneLen)
#' 
#' @export tpm
tpm <- function(x, gene.length) {
  x <- as.matrix(x)
  len.norm.lib.size <- colSums(x / gene.length)
  return((t(t(x) / len.norm.lib.size) * 1e06) / gene.length)
}

#' Convert a RPKM matrix to a TPM matrix
#' 
#' 
#' @param x A count matrix or other objects that can be converted to a matrix
#' by \code{as.matrix}
#' @return transcripts per million (TPM) values
#' @seealso \code{\link{rpkm2tpm}}
#' @examples
#' 
#' testMatrix <- matrix(rnbinom(200, size=5, prob=0.1), nrow=20, ncol=10)
#' testMatrixGeneLen <- as.integer(10^rnorm(20, mean=3, sd=0.5))
#' testMatrixTpm <- tpm(testMatrix, testMatrixGeneLen)
#' testMatrixRpkm <- edgeR::rpkm(testMatrix, testMatrixGeneLen)
#' testthat::expect_equal(testMatrixTpm, rpkm2tpm(testMatrixRpkm))
#' 
#' @export rpkm2tpm
rpkm2tpm <- function(x) {
  x <- as.matrix(x)
  return(t(t(x)/colSums(x))*10^6)
}
