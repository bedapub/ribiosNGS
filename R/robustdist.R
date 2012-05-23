robustDist <- function(x,...) {
  res <- dist(x,...)
  res.max <- max(res, na.rm=TRUE)
  if(is.na(res.max)) res.max <- 1L
  res[is.na(res)] <- res.max
  return(res)
}
