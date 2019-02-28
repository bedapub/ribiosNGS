setGeneric("rowscale", function(object, center, scale) standardGeneric("rowscale"))

#' Scale a matrix by row
#' 
#' Scaling a matrix by row can be slightly slower due to a transposing step.
#' 
#' @param object An matrix
#' @param center Logical, passed to \code{scale}. If missing the value is set
#' to \code{TRUE}
#' @param scale Logical, passed to \code{scale}. If missing the value is set to
#' \code{TRUE}
#' @return A matrix with each row scaled.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @seealso \code{\link{scale}}
#' @keywords ~kwd1 ~kwd2
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
setMethod("rowscale", c("matrix","ANY", "ANY"), function(object, center, scale) {
  if(missing(center))
    center <- TRUE
  if(missing(scale))
    scale <- TRUE
  t(scale(t(object),center=center, scale=scale))
})
