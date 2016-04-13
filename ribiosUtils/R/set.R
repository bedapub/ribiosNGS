mset <- function(func, ...) {
  li <- list(...)
  if(length(li)==1L) li <- li[[1L]]
  if(length(li)==1L) return(li[[1]])
  
  res <- do.call(func, list(li[[1L]], li[[2L]]))
  if(length(li)>2L)
    for(i in 3L:length(li))
      res <- do.call(func, list(res, li[[i]]))
  res
}
munion <- function(...) mset(func=union, ...)
mintersect <- function(...) mset(func=intersect, ...)
msetdiff <- function(...) mset(func=setdiff, ...)

#' Reverse setdiff
#'
#' @param x a vector
#' @param y another vector
#' @return Similar to setdiff, but with elements in y but not in x
#' @details reverse setdiff, i.e. rsetdiff(x,y) equals setdiff(y,x)
#' @author Jitao David Zhang
#' @examples
#' testVec1 <- LETTERS[3:6]
#' testVec2 <- LETTERS[5:7]
#' rsetdiff(testVec1, testVec2)
rsetdiff <- function(x,y) setdiff(y,x)

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
