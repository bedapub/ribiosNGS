#' Make a vector free of NA and unique
#' @param x A vector
#' @return A unique vector without NA
#' @examples 
#' testVec <- c(3,4,5,NA,3,5)
#' uniqueNonNA(testVec)
uniqueNonNA <- function(x) {
  x <- x[!is.na(x)]
  res <- unique(x)
  return(res)
}
