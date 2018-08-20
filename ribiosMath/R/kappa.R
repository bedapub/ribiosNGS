#' Calculate column-wise kappa statistics of a matrix, using a simple procedure by going through the matrix and counting
#'
#' @param matrix a binary matrix of either 0 or one
#' @param minOverlap Numeric/integer, the minimal overlap between two columns to be considered for further calculation
#' 
#' @return
#' A matrix of size nxn if the input matrix is of size mxn (m is arbitrary)
#'
#' @family kappa functions
#' @seealso \code{\link{colKappa}} to calculate the same statistic using a linear algebra based routine
#' 
#' @examples
#' testMat <- cbind(c(1,1,0,0,1,0), c(1,1,0,1,1,0))
#' ribiosMath:::kappaSimp(testMat)
kappaSimp <- function(matrix, minOverlap=0) {
    res <- .Call('_ribiosMath_colKappaSimp', matrix, minOverlap)
    return(res)
}

