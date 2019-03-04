#' S3 method for row-scaling
#' @param x Any object
#' @param center Logical, whether centering should be done before scaling
#' @param scale Logical, whether scaling should be done
#' @export rowscale
rowscale <- function(x, center = TRUE, scale=TRUE) UseMethod("rowscale")

#' Scale a matrix by row
#' 
#' Scaling a matrix by row can be slightly slower due to a transposing step.
#' 
#' @param x An matrix
#' @param center Logical, passed to \code{scale}. 
#' to \code{TRUE}
#' @param scale Logical, passed to \code{scale}. 
#' \code{TRUE}
#' @return A matrix with each row scaled.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @seealso \code{\link{scale}}
#' @examples
#' 
#' mat <- matrix(rnorm(20), nrow=4)
#' rs.mat <- rowscale(mat)
#' 
#' print(mat)
#' print(rs.mat)
#' rowMeans(rs.mat)
#' apply(rs.mat, 1L, sd)
#' 
#' rowscale(mat, center=FALSE, scale=FALSE) ## equal to mat
#' rowscale(mat, center=TRUE, scale=FALSE)
#' rowscale(mat, center=FALSE, scale=TRUE)
#' 
#' @method rowscale matrix
#' @export
rowscale.matrix <- function(x, center=TRUE, scale=TRUE) {
  t(scale(t(x),center=center, scale=scale))
}
