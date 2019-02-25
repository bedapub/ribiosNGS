##------------##
## S3: PCAScoreMatrix
##------------##

#' Construct a S3-class PCAScoreMatrix object
#' 
#' @param scoreMatrix Numeric matrix, objects in rows and PCs in columns
#' @param expVar Numeric vector, length must equal the number of columns of \code{scoreMatrix}, explained variance by respective PCs
#' 
#' @return A S3-class \code{PCAScoreMatrix} object, which is a score matrix with explained variances (expVar) as attribute.
#' 
#' @seealso \code{as.matrix.PCAScoreMatrix}, \code{expVar.PCAScoreMatrix}, \code{print.PCAScoreMatrix}.
#' This function is usually not called by the end user; instead, it is used by the function \code{\link{pcaScores}}
#' 
#' @examples 
#' myPCmat <- PCAScoreMatrix(matrix(rnorm(15),ncol=3), c(0.25, 0.15, 0.1))
#' myPCmat
PCAScoreMatrix <- function(scoreMatrix, expVar) {
  stopifnot(ncol(scoreMatrix)==length(expVar))
  attr(scoreMatrix, "expVar") <- expVar
  class(scoreMatrix) <- "PCAScoreMatrix"
  return(scoreMatrix)
}

#' Coerece a PCAScoreMatrix into score matrix
#' 
#' @param x A \code{PCAScoreMatrix} S3 object
#' @param ... Currently ignored
#' 
#' @return A numeric matrix, the score matrix
#' 
#' @examples
#' myPCmat <- PCAScoreMatrix(matrix(rnorm(15),ncol=3), c(0.25, 0.15, 0.1))
#' as.matrix(myPCmat)
as.matrix.PCAScoreMatrix <- function(x, ...) {
  attr(x, "expVar") <- NULL
  class(x) <- "matrix"
  return(x)
}


#' Print PCAScoreMatrix
#' @param x A \code{PCAScoreMatrix} S3-object
#' @param ... Ignored
#' 
#' @return NULL, side effect is used
#' 
#' @examples 
#' myPCmat <- PCAScoreMatrix(matrix(rnorm(15),ncol=3), c(0.25, 0.15, 0.1))
#' myPCmat
print.PCAScoreMatrix <- function(x, ...) {
  expVar <- attr(x, "expVar")
  cat(sprintf("PCAScoreMatrix with %d dimensions\n", length(expVar)))
  show(as.matrix(x))
  cat("Explained variances:", paste(ribiosUtils::percentage(expVar), 
                                    collapse=","),
      sprintf("(%s in total)\n", ribiosUtils::percentage(sum(expVar))))
  cat("Options\n")
  cat("-- Use 'as.matrix' to turn this object into a simple matrix\n")
  cat("-- Use 'expVar' to extract explained variances\n")
  cat("-- Use 'expVarLabel' to generate labels of explained variances")
}