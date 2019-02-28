#' @export mset
mset <- function(func, ...) {
  li <- list(...)
  if(length(li)==0L) return(li)
  if(length(li)==1L) li <- li[[1L]]
  if(length(li)==1L) return(unique(li[[1]]))
  
  res <- do.call(func, list(li[[1L]], li[[2L]]))
  if(length(li)>2L) {
    for(i in 3L:length(li))
      res <- do.call(func, list(res, li[[i]]))
  } 
  res
}

#' Operations for multiple sets
#' 
#' Set operation functions in the \code{base} package, \code{union},
#' \code{intersect} and \code{setdiff}, can only be applied to binary
#' manipulations involving two sets. Following functions, \code{munion},
#' \code{mintersect} and \code{msetdiff}, extend their basic versions to deal
#' with multiple sets.
#' 
#' These functions apply set manipulations (union, intersect, or difference) in
#' a sequential manner: the first two sets are considered first, then the
#' third, the fourth and so on, till all sets have been visited.
#' 
#' @aliases mset munion mintersect msetdiff
#' @param \dots Vectors of items, or a list of them. See examples below.
#' @return A vector of set operation results. Can be an empty vector if no
#' results were returned.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @seealso \code{\link{union}}, \code{\link{intersect}} and
#' \code{\link{setdiff}}.
#' @examples
#' 
#' set1 <- c("HSV", "FCB", "BVB", "FCN", "HAN")
#' set2 <- c("HSV", "FCB", "BVB", "HAN")
#' set3 <- c("HSV", "BVB", "FSV")
#' 
#' munion(set1, set2, set3)
#' mintersect(set1, set2, set3)
#' msetdiff(set1, set2, set3)
#' 
#' ## sets can be given in a list as well
#' munion(list(set1, set2, set3))
#' mintersect(list(set1, set2, set3))
#' msetdiff(list(set1, set2, set3))
#' 
#' @export munion
munion <- function(...) mset(func=union, ...)

#' @export mintersect
mintersect <- function(...) mset(func=intersect, ...)

#' @export msetdiff
msetdiff <- function(...) mset(func=setdiff, ...)

#' Reverse setdiff
#' 
#' reverse setdiff, i.e. rsetdiff(x,y) equals setdiff(y,x)
#' 
#' @param x a vector
#' @param y another vector
#' @return Similar to setdiff, but with elements in y but not in x
#' @author Jitao David Zhang
#' @examples
#' 
#' testVec1 <- LETTERS[3:6]
#' testVec2 <- LETTERS[5:7]
#' rsetdiff(testVec1, testVec2)
#' 
#' @export rsetdiff
rsetdiff <- function(x,y) setdiff(y,x)


#' Compare two vectors by set operations
#' 
#' Basic set operations are used to compare two vectors
#' 
#' @param vec1 A vector of atomic types, e.g. integers, characters, etc.
#' @param vec2 A vector of the same type as \code{vec1}
#' @return A vector of six integer elements \item{vec1.setdiff }{Number of
#' unique items only in \code{vec1} but not in \code{vec2}} \item{intersect
#' }{Number of items in both \code{vec1} and \code{vec2}} \item{vec2.setdiff
#' }{Number of unique items only in \code{vec2} but not in \code{vec1}}
#' \item{vec1.ulen}{Number of unique items in \code{vec1}}
#' \item{vec2.ulen}{Number of unique items in \code{vec2}} \item{union}{Number
#' of unique items in \code{vec1} and \code{vec2}}
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @examples
#' 
#' year1 <- c("HSV", "FCB", "BVB", "S04", "FCN")
#' year2 <- c("HSV", "FCK", "S04")
#' compTwoVecs(year1, year2)
#' 
#' @export compTwoVecs
compTwoVecs <- function(vec1, vec2) {
  vec1.ulen <- ulen(vec1)
  vec2.ulen <- ulen(vec2)
  vec1.setdiff <- length(setdiff(vec1, vec2))
  common <- length(intersect(vec1, vec2))
  vec2.setdiff <- length(setdiff(vec2, vec1))
  unionLen <- length(union(vec1, vec2))
  return(c("vec1.setdiff"=vec1.setdiff,
           "intersect"=common,
           "vec2.setdiff"=vec2.setdiff,
           "vec1.ulen"=vec1.ulen,
           "vec2.ulen"=vec2.ulen,
           "union"=unionLen))
}
