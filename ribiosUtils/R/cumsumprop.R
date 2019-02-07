#' Proportion of cumulative sum over sum
#' 
#' @param x Numeric vector
#' @return the proportion cumulative sum over sum
#' 
#' @examples
#' x <- 1:4
#' cumsumprop(x) ## 0.1, 0.3, 0.6, 1
cumsumprop <- function(x) cumsum(x)/sum(x)

#' Sort a numeric vector and filter by a threshold of cumsumprop
#' @param x Numeric vector, usually named
#' @return Another numeric vector, likely shorter than x, with only items whose \code{cumsumprop} is equal or lower than \code{thr}. The rest items are summed into one new item, with the name \code{rest}
#' 
#' @aliases \code{eightytwenty}
#' 
#' This function can be useful to extract from a long numeric vector the largest items that dominate the sum of the vector
#' 
#' @examples
#' x <- c("A"=1,"B"=2,"C"=3,"D"=4,"E"=400,"F"=500)
#' sortAndFilterByCumsumprop(x, thr=0.99) ## F and E should be returned
sortAndFilterByCumsumprop <- function(x, thr=.9) {
  xs <- sort(x, decreasing=TRUE)
  xprop <- cumsumprop(xs)
  isSel <- xprop <= thr
  res <- c(xs[isSel], rest=sum(xs[!isSel]))
  return(res)
}
