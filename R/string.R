countTokens <- function(str, split="\t",...) {
  str <- as.character(str)
  sapply(strsplit(str, split, ...), length)
}
nField <- function(str, split="\t",...) {
  .Deprecated("countTokens", package="ribiosUtils")
  countTokens(str=str, split=split,...)
}


strtoken <- function(x, split, index, ...) {
  x <- as.character(x)
  n <- length(x)
  s <- strsplit(x, split = split, ...)
  nc <- unlist(lapply(s, length))
  out <- matrix("", n, max(nc))
  for (i in 1:n) {
    if (nc[i]) 
      out[i, 1:nc[i]] <- s[[i]]
  }
  if(!missing(index) && !is.null(index))
    out <- out[,index]
  out
}
