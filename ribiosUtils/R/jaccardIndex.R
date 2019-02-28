#' Calculate the Jaccard Index between two vectors
#' 
#' Calculate the Jaccard Index between two vectors
#' 
#' 
#' @aliases jaccardIndex jaccardDistance
#' @param x A vector
#' @param y A vector
#' @return The Jaccard Index, a number between 0 and 1
#' 
#' \code{JaccardDistance} is defined as \code{1-JaccardIndex}.
#' @examples
#' 
#' myX <- 1:6
#' myY <- 4:9
#' jaccardIndex(myX, myY)
#' jaccardDistance(myX, myY)
#' 
#' myX <- LETTERS[1:5]
#' myY <- LETTERS[6:10]
#' jaccardIndex(myX, myY)
#' jaccardDistance(myX, myY)
#' 
#' @export jaccardIndex
jaccardIndex <- function(x,y) length(intersect(x,y))/length(union(x,y))

#' @rdname jaccardIndex
jaccardDistance <- function(x,y) {
  return(1 - jaccardIndex(x, y))
}





#' Calculate pairwise distances between each pair of items in a list
#' 
#' Calculate pairwise distances between each pair of items in a list
#' 
#' 
#' @param list A list
#' @param fun A function that receives two vectors (such as jaccardIndex) and
#' returns a number (scale)
#' @return A symmetric matrix of dimension \code{mxm}, where \code{m} is the
#' length of the list
#' @examples
#' 
#' myList <- list(first=LETTERS[3:5], second=LETTERS[1:3], third=LETTERS[1:5], fourth=LETTERS[6:10])
#' pairwiseDist(myList, fun=jaccardIndex)
#' ## despite of the name, any function that returns a number can work
#' pairwiseDist(myList, fun=jaccardDistance)
#' 
pairwiseDist <- function(list, fun=jaccardIndex) {
  len <- length(list)
  res <- matrix(0, len, len)
  colnames(res) <- rownames(res) <- names(list)
  vals <- sapply(seq(from = 1, to = len - 1), function(i) {
    sapply(seq(from = i + 1, to = len), function(j) {
      do.call(fun, list(list[[i]], list[[j]]))
    })
  })
  vv <- unlist(vals)
  res[lower.tri(res)] <- vv
  res <- t(res) + res
  diag(res) <- do.call(fun, list(list[[1]], list[[1]]))
  return(res)
}





#' Calculate pairwise Jaccard Indices between each pair of items in a list
#' 
#' Calculate pairwise Jaccard Indices between each pair of items in a list
#' 
#' 
#' @aliases pairwiseJaccardIndex pairwiseJaccardDistance
#' @param list A list
#' @return A symmetric matrix of dimension \code{mxm}, where \code{m} is the
#' length of the list
#' 
#' \code{pairwiseJaccardDistance} is defined as \code{1-pairwiseJaccardIndex}.
#' @examples
#' 
#' myList <- list(first=LETTERS[3:5], second=LETTERS[1:3], third=LETTERS[1:5], fourth=LETTERS[6:10])
#' pairwiseJaccardIndex(myList)
#' 
#' poormanPJI <- function(list) {
#'   sapply(list, function(x) sapply(list, function(y) jaccardIndex(x,y)))
#' }
#' stopifnot(identical(pairwiseJaccardIndex(myList), poormanPJI(myList)))
#' 
#' @export pairwiseJaccardIndex
pairwiseJaccardIndex <- function(list) {
  return(pairwiseDist(list, fun=jaccardIndex))
}

#' @rdname pairwiseJaccardIndex
pairwiseJaccardDistance <- function(list) {
  return(pairwiseDist(list, fun=jaccardDistance))
}





#' Cumulative Jaccard Index
#' 
#' Cumulative Jaccard Index
#' 
#' 
#' @aliases cumJaccardIndex cumJaccardDistance
#' @param list A list of characters or integers
#' @return The cumulative Jaccard Index, a vector of values between 0 and 1, of
#' the same length as the input list
#' 
#' The cumulative Jaccard Index is calculated by calculating the Jaccard Index
#' of element \code{i} and the union of elements between \code{1} and
#' \code{i-1}. The cumulative Jaccard Index of the first element is set as 0.0.
#' 
#' The cumulative Jaccard distance is defined in almost the same way, with the
#' only difference the distance is returned. The value of the first element is
#' 1.0.
#' @note An advantage of using cumulative overlap coefficient over cumulative
#' Jaccard Index is that it is monotonic: the value is garanteed to decrease
#' from 1 to 0, whereas the cumulative Jaccard Index may not be monotic.
#' @seealso \code{\link{cumOverlapCoefficient}}
#' @examples
#' 
#' myList <- list(first=LETTERS[1:5], second=LETTERS[6:10], third=LETTERS[8:12], fourth=LETTERS[1:12])
#' cumJaccardIndex(myList)
#' cumJaccardDistance(myList)
#' 
#' @export cumJaccardIndex
cumJaccardIndex <- function(list) {
  res <- numeric(length(list))
  cumvec <- list[[1]]
  res[1] <- 0
  for(i in 2:length(res)) {
    res[i] <- jaccardIndex(list[[i]], cumvec)
    cumvec <- union(cumvec, list[[i]])
  }
  return(res)
}

#' @rdname cumJaccardIndex
cumJaccardDistance <- function(list) {
  res <- 1 - cumJaccardIndex(list)
  return(res)
}





#' Overlap coefficient, also known as Szymkiewicz-Simpson coefficient
#' 
#' Overlap coefficient, also known as Szymkiewicz-Simpson coefficient
#' 
#' 
#' @aliases overlapCoefficient overlapDistance
#' @param x A vector
#' @param y A vector
#' @param checkUniqueNonNA Logical, if \code{TRUE}, \code{x} and \code{y} are
#' made unique and non-NA
#' @return The overlap coefficient
#' @seealso \code{\link{jaccardIndex}}
#' 
#' \code{overlapCofficient} calculates the overlap coefficient, and
#' \code{overlapDistance} is defined by 1-\code{overlapCoefficient}.
#' @examples
#' 
#' myX <- 1:6
#' myY <- 4:9
#' overlapCoefficient(myX, myY)
#' 
#' myY2 <- 4:10
#' overlapCoefficient(myX, myY2)
#' ## compare the result with Jaccard Index
#' jaccardIndex(myX, myY2)
#' 
#' ## overlapDistance
#' overlapDistance(myX, myY2)
#' 
#' @export overlapCoefficient
overlapCoefficient <- function(x,y, checkUniqueNonNA=FALSE) {
  if(checkUniqueNonNA) {
    x <- uniqueNonNA(x)
    y <- uniqueNonNA(y)
  }
  res <- length(intersect(x,y))/pmin(length(x), length(y))
  return(res)
}

#' @rdname overlapCoefficient
overlapDistance <- function(x,y, checkUniqueNonNA=FALSE) {
  return(1 - overlapCoefficient(x, y, checkUniqueNonNA = checkUniqueNonNA))
}





#' Calculate pairwise overlap coefficients between each pair of items in a list
#' 
#' Calculate pairwise overlap coefficients between each pair of items in a list
#' 
#' 
#' @aliases pairwiseOverlapDistance pairwiseOverlapCoefficient
#' @param list A list
#' @return A symmetric matrix of dimension \code{mxm}, where \code{m} is the
#' length of the list
#' 
#' \code{pairwiseOverlapDistance} is defined the pairwise overlap distance.
#' @examples
#' 
#' myList <- list(first=LETTERS[3:5], second=LETTERS[1:3], third=LETTERS[1:5], fourth=LETTERS[6:10])
#' pairwiseOverlapCoefficient(myList)
#' pairwiseOverlapDistance(myList)
#' 
#' poormanPOC <- function(list) {
#'   sapply(list, function(x) sapply(list, function(y) overlapCoefficient(x,y)))
#' }
#' stopifnot(identical(pairwiseOverlapCoefficient(myList), poormanPOC(myList)))
#' 
pairwiseOverlapDistance <- function(list) {
  return(pairwiseDist(list, fun=overlapDistance))
}

#' @rdname pairwiseOverlapDistance
pairwiseOverlapCoefficient <- function(list) {
  return(pairwiseDist(list, fun=overlapCoefficient))
}






#' Cumulative overlap coefficient
#' 
#' Cumulative overlap coefficient
#' 
#' 
#' @aliases cumOverlapCoefficient cumOverlapDistance
#' @param list A list of characters or integers
#' @return The cumulative overlap coefficients, a vector of values between 0
#' and 1, of the same length as the input list
#' 
#' The cumulative overlap coefficient is calculated by calculating the overlap
#' coefficient of element \code{i} and the union of elements between \code{1}
#' and \code{i-1}. The cumulative overlap coefficient of the first element is
#' set as 0.0.
#' 
#' The cumulative overlap distance is defined in almost the same way, with the
#' only difference the distance is returned. The value of the first element is
#' 1.0. Pratically it is calculated by \code{1-cumOverlapCoefficient}.
#' 
#' Since the denominator of the overlap coefficient is the size of the smaller
#' set of the two, which is bound to be the size of element \code{i}, the
#' cumulative overlap distance can be interpreted as the proportion of new
#' items in each new element that are unseen in previous elements. Similarly,
#' the cumulative overlap coefficient can be interpreted as the proportion of
#' items in each new element that have been seen in previous elements. See
#' examples below.
#' @note An advantage of using cumulative overlap coefficient over cumulative
#' Jaccard Index is that it is monotonic: the value is garanteed to decrease
#' from 1 to 0, whereas the cumulative Jaccard Index may not be monotic.
#' @examples
#' 
#' myList <- list(first=LETTERS[1:5], second=LETTERS[6:10], third=LETTERS[8:12], fourth=LETTERS[1:12])
#' cumOverlapCoefficient(myList)
#' cumOverlapDistance(myList)
#' 
#' @export cumOverlapCoefficient
cumOverlapCoefficient <- function(list) {
  res <- numeric(length(list))
  cumvec <- list[[1]]
  res[1] <- 0.0
  for(i in 2:length(res)) {
    res[i] <- overlapCoefficient(list[[i]], cumvec)
    cumvec <- union(cumvec, list[[i]])
  }
  return(res)
}

#' @rdname cumOverlapCoefficient
cumOverlapDistance <- function(list) {
  res <- 1 - cumOverlapCoefficient(list)
  return(res)
}
