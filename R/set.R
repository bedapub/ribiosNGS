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

