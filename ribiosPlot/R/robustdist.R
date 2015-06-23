robustDist <- function(x,...) {
  res <- dist(x,...)
  isBad <- is.na(res) | is.nan(res) | is.infinite(res)
  
  res.max <- max(res[!isBad], na.rm=TRUE)
  if(is.na(res.max)) res.max <- 1L

  res[isBad] <- res.max
  return(res)
}
