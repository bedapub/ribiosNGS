#' Calculate the Jaccard Index between two vectors
#' 
#' @param x A vector
#' @param y A vector
#' @return The Jaccard Index, a number between 0 and 1
#' 
#' @examples 
#' myX <- 1:6
#' myY <- 4:9
#' jaccardIndex(myX, myY)
#' 
#' myX <- LETTERS[1:5]
#' myY <- LETTERS[1:10]
#' jaccardIndex(myX, myY)
jaccardIndex <- function(x,y) length(intersect(x,y))/length(union(x,y))

#' Calculate pairwise Jaccard Indices between each pair of items in a list
#' 
#' @param list A list
#' @return A symmetric matrix of dimension \code{mxm}, where \code{m} is the length of the list
#' 
#' @examples 
#' myList <- list(first=LETTERS[3:5], second=LETTERS[1:3], third=LETTERS[1:5], fourth=LETTERS[6:10])
#' pairwiseJaccardIndex(myList)
#' 
#' poormanPJI <- function(list) {
#'   sapply(list, function(x) sapply(list, function(y) jaccardIndex(x,y)))
#' }
#' stopifnot(identical(pairwiseJaccardIndex(myList), poormanPJI(myList)))
pairwiseJaccardIndex <- function(list) {
  len <- length(list)
  res <- matrix(0, len, len)
  colnames(res) <- rownames(res) <- names(list)
  vals <- sapply(seq(from=1, to=len-1), function(i) {
    sapply(seq(from=i+1, to=len), function(j) {
      jaccardIndex(list[[i]], list[[j]])
    })
  })
  vv <- unlist(vals)
  ## fill the lower triangle, and make the matrix symmetric
  res[lower.tri(res)] <- vv
  res <- t(res) + res
  diag(res) <- 1
  return(res)
}

