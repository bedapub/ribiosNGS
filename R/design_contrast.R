#' Check dimensionality of design matrix
#'
#' @param nsample Integer, number of samples
#' @param design Design matrix
#' @return Side effect is used: the function stops if sample size does not equal ncol(matrix)
#' @examples
#' nsample <- 4
#' design <- matrix(1:20, ncol=5)
#' assertDesign(nsample, design)
assertDesign <- function(nsample, design) {
  if(!is.numeric(nsample) | length(nsample)!=1)
    stop('nsample must be an integer representing the sample size')
  if(nsample != nrow(design))
    stop("Number of rows of the design matrix (",
         nrow(design), ") must equal the sample size (", nsample, ")")
}

#' Check dimensionality of contrast matrix
#'
#' @param design Design matrix
#' @param contrast Contrast matrix
#' @return Side effect is used: the function stops if the ncol(design) does not equal nrow(contrast)
#' @examples
#' design <- matrix(1:20, ncol=5)
#' contrast <- matrix(c(-1,1,0,0,0, 0,1,0,-1,0), nrow=5)
#' assertContrast(design, contrast)

assertContrast <- function(design, contrast) {
  if(ncol(design)!=nrow(contrast))
    stop("Number of rows of the contrast matrix (",
         nrow(contrast), ")must equal the number of columns in the design matrix (",
         ncol(design), ")")
}

#' Check dimensionality of both design and contrast matrix
#'
#' @param nsample Integer, number of samples
#' @param design Design matrix
#' @param contrast Contrast matrix
#' @return Side effect is used: the function stops if there are errors in the dimensionalities
#' @seealso \code{\link{assertDesign}}, \code{\link{assertContrast}}
#' @examples
#' nsample <- 4
#' design <- matrix(1:20, ncol=5)
#' contrast <- matrix(c(-1,1,0,0,0, 0,1,0,-1,0), nrow=5)
#' assertDesignContrast(nsample, design, contrast)

assertDesignContrast <- function(nsample, design, contrast) {
  assertDesign(nsample, design)
  assertContrast(design, contrast)
}
