#' Calculate column-wise kappa statistics of a matrix, using linear algebra procedure
#'
#' @param matrix a binary matrix of either 0 or one
#'
#' @return
#' A matrix of size nxn if the input matrix is of size mxn (m is arbitrary)
#'
#' @family kappa functions
#' @seealso \code{\link{kappaSimp}} to calculate the same statistic using a simpler routine
#' 
#' @examples
#' testMat <- cbind(c(1,1,0,0,1,0), c(1,1,0,1,1,0))
#' kappaLA(testMat)
kappaLA <- function(matrix) {
    res <- .Call("kappaLA", matrix)
    return(res)
}

#' Calculate column-wise kappa statistics of a matrix, using a simple procedure by going through the matrix and counting
#'
#' @param matrix a binary matrix of either 0 or one
#'
#' @return
#' A matrix of size nxn if the input matrix is of size mxn (m is arbitrary)
#'
#' #' @family kappa functions
#' @seealso \code{\link{kappaLA}} to calculate the same statistic using a linear algebra based routine
#' 
#' @examples
#' testMat <- cbind(c(1,1,0,0,1,0), c(1,1,0,1,1,0))
#' kappaSimp(testMat)
kappaSimp <- function(matrix) {
    res <- .Call("kappaSimp", matrix)
    return(res)
}

