## print a vector as an input string
inputVec <- function(vec, verbose=TRUE) {
  res <- paste("c(", paste(sprintf("\"%s\"",vec), collapse=","),")\n", sep="")
  if(verbose) {
    cat(res)
  }
  return(invisible(res))
}
