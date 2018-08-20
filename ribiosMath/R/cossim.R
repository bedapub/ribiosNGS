#' Calculate the cosine similarity between two vectors (matrices)
#' 
#' @param x An integer or numeric vector or matrix
#' @param y An integer or numeric vector or matrix
#' @param na.rm Logical, whether \code{NA} should be removed
#' 
#' @details 
#' If given as vectors, \code{x} and \code{y} must be of the same
#' length. If given as matrices, both must have the same number of
#' rows. If given as a pair of matrix and vector, the length of the
#' vector must match the row number of the matrix. Otherwise the 
#' function aborts and prints error message.
#' 
#' If parameters are given as matrices, the function calculates the cossine similarity between all pair of \emph{columns} of both matrices.
#' 
#' If \code{na.rm} is set \code{FALSE}, any \code{NA} in the input vectors
#' will cause the result to be \code{NA}, or \code{NaN} if all values
#' turn out to be \code{NA}.
#' 
#' @return Numeric vector or matrix, the cossine similarity between the inputs
#' 
#' @references \url{http://en.wikipedia.org/wiki/Cosine_similarity}
#' @note Currently, \code{na.rm} is only considered when both inputs are vectors
#' @author Jitao David Zhang <jitao_david.zhang@roche.com>
#' @seealso \code{\link{cor}}, \code{\link{cosdist}}
#' @examples 
#' testVal1 <- rnorm(10)
#' testVal2 <- rnorm(10)
#' testVal3 <- c(rnorm(9), NA)
#' 
#' cossim(testVal1, testVal2)
#' cossim(testVal1, testVal3, na.rm=TRUE)
#' cossim(testVal1, testVal3, na.rm=FALSE)
#' 
#' cosdist(testVal1, testVal2)
#' cosdist(testVal1, testVal3, na.rm=TRUE)
#' cosdist(testVal1, testVal3, na.rm=FALSE)

#' ## test matrix
#' testMat1 <- matrix(rnorm(1000), nrow=10)
#' testMat2 <- matrix(rnorm(1000), nrow=10)
#' system.time(testMatCos <- cossim(testMat1, testMat2))
#' 
#' testMatVec <- cossim(testMat1, testMat2[,1L])
#' testVecMat <- cossim(testMat1[,1L], testMat2)
#' 
#' @export
cossim <- function(x,y, na.rm=TRUE) {
  if(missing(y)) y <- x
  xIsMatrix <- is.matrix(x)
  yIsMatrix <- is.matrix(y)
  if(!xIsMatrix && !yIsMatrix) {
    return(cossimVec(x, y, na.rm))
  } else if(xIsMatrix && !yIsMatrix) {
    y <- matrix(y, ncol=1L)
  } else if(!xIsMatrix && yIsMatrix) {
    x <- matrix(x, ncol=1L)
  }
  if(nrow(x)!=nrow(y))
    stop("incompatible dimension")

  aa <- t(x) %*% y
  bb <- apply(x, 2L, function(xx) sqrt(sum(xx^2)))
  cc <- rep(apply(y, 2L, function(yy) sqrt(sum(yy^2))),
            each=nrow(aa))
  res <- aa/bb/cc
  if(! (xIsMatrix && yIsMatrix))
    res <- as.vector(res)
  return(res)
}

#' Calculate the cosine distance between two vectors (matrices)
#' 
#' @param x An integer or numeric vector or matrix
#' @param y An integer or numeric vector or matrix
#' @param na.rm Logical, whether \code{NA} should be removed
#' 
#' @details 
#' Cossine distance is defined by \eqn{1-cossim}, where \eqn{cossim} represents the cosine similarity.
#' 
#' If parameters are given as matrices, the function calculates the cossine distance between all pair of \emph{columns} of both matrices.
#' 
#' @return Numeric vector or matrix, the cossine similarity between the inputs
#' 
#' @references \url{http://en.wikipedia.org/wiki/Cosine_similarity}
#' @note Currently, \code{na.rm} is only considered when both inputs are vectors
#' @author Jitao David Zhang <jitao_david.zhang@roche.com>
#' @seealso \code{\link{cor}}, \code{\link{cossim}}
#' @examples 
#' testVal1 <- rnorm(10)
#' testVal2 <- rnorm(10)
#' testVal3 <- c(rnorm(9), NA)
#' 
#' cosdist(testVal1, testVal2)
#' cosdist(testVal1, testVal3, na.rm=TRUE)
#' cosdist(testVal1, testVal3, na.rm=FALSE)

#' ## test matrix
#' testMat1 <- matrix(rnorm(1000), nrow=10)
#' testMat2 <- matrix(rnorm(1000), nrow=10)
#' 
#' testVecMatDist1 <- cosdist(testMat1[,1L], testMat2)
#' testVecMatDist <- cosdist(testMat1, testMat2)
#' @export
cosdist <- function(x,y, na.rm=TRUE) {
  1-cossim(x=x, y=y, na.rm=na.rm)
}
