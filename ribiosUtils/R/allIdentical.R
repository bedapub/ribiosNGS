#' Testing whether several objects are all identical with each other
#' 
#' Given several objects, the function tests whether all of them are identical.
#' 
#' 
#' @param \dots Objects to be tested. Can be given as a list, or simplying
#' appending names separated by commas, see example.
#' @return Logical, whether all objects are the same
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @seealso \code{\link{identical}}
#' @examples
#' 
#' test1 <- test2 <- test3 <- LETTERS[1:3]
#' allIdentical(test1, test2, test3)
#' allIdentical(list(test1, test2, test3))
#' 
#' num1 <- num2 <- num3 <- num4 <- sqrt(3)
#' allIdentical(num1, num2, num3, num4)
#' 
#' @export allIdentical
allIdentical <- function(...) {
  x <- list(...)
  if(length(x)==1) x <- x[[1L]]
  stopifnot(length(x)>=2L)
  res <- identical(x[[1]], x[[2]])
  if(length(x)>2) {
    for(i in 3:length(x)) {
      res <- identical(x[[i]], x[[i-1]]) && res
      if(!res) return(FALSE)
    }
  }
  return(res)
}
