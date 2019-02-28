#' Length of unique elements in a vector
#' 
#' 
#' @aliases uniqueLength ulen
#' @param x A vector
#' @param incomparables See \code{\link{unique}}
#' @return An integer indicating the number of unique elements in the input
#' vector
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @seealso \code{unique}
#' @examples
#' 
#' test.vec1 <- c("HSV", "FCB", "BVB", "HSV", "BVB")
#' uniqueLength(test.vec1)
#' 
#' test.vec2 <- c(1L, 2L, 3L, 5L, 3L, 4L, 2L, 1L, 5L)
#' ulen(test.vec2)
#' 
#' @export uniqueLength
uniqueLength <- function(x, incomparables=FALSE)
  length(unique(x,incomparables=incomparables))

#' Print the chosen few items of a long vector
#' 
#' Print the chosen few (the first and the last) items of a long vector
#' 
#' 
#' @param vec A vector of characters or other types that can be cast into
#' characters
#' @param start Integer, how many elements at the start shall be printed
#' @param end Integer, how many elements at the end shall be printed
#' @param collapse Character used to separate elements
#' @return A character string ready to be printed
#' @note In case the vector is shorter than the sum of \code{start} and
#' \code{end}, the whole vector is printed.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @examples
#' 
#' lvec1 <- 1:100
#' chosenFew(lvec1)
#' chosenFew(lvec1, start=5, end=3)
#' 
#' svec <- 1:8
#' chosenFew(svec)
#' chosenFew(svec, start=5, end=4)
#' 
#' @export chosenFew
chosenFew <- function(vec, start=3, end=1, collapse=",") {
  if(length(vec)<start+end)
    return(paste(vec, collapse=collapse))
  vlen <- length(vec)
  return(paste(paste(vec[1:start], collapse=collapse),
               "...",
               paste(vec[(vlen-end+1):vlen], collapse=","),
               sep=collapse))
}

#' @export ulen
ulen <- uniqueLength

