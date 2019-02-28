#' Replace NA with FALSE
#' 
#' Replace \code{NA} in a vector with \code{FALSE}
#' 
#' 
#' @param x A logical vector or matrix
#' @return Logical vector or matrix with NAs replaced by FALSE
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @seealso myX <- c("HSV", "FCK", "FCN", NA, "BVB") res <- myX == "HSV"
#' na.false(res)
#' @export na.false
na.false <- function(x) {x[is.na(x)] <- FALSE; return(x)}
