isBounded <- function(x, lb, ub) {
  x>= lb & x<= ub
}

progress <- function(x, verbose=TRUE) {
  if(verbose) {
    timestamp <- format(Sys.time(), "%H:%M:%S")
    progress.string <- sprintf("[%s] %s\n", timestamp, x)
    cat(progress.string)
  }
  invisible(NULL)
}
