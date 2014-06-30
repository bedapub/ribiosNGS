fisherP <- function(...) {
  li <- list(...)
  if(length(li[[1]])==1)
    li <- li[[1]]
  if(length(li)<2 || ulen(sapply(li, length))!=1)
    stop("Input must be a list of p values of the same length")
  mat <- do.call(cbind, li)
  sums <- apply(mat, 1, function(x) -2*sum(log(x)))
  pval <- pchisq(sums, df=2*ncol(mat), lower.tail=FALSE)
}
