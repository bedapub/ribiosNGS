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
