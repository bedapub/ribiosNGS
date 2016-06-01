#' @importFrom grDevices dev.off pdf
#' @importFrom stats reshape
#' @importFrom utils read.table write.table

#' @export
isBounded <- function(x, lb, ub) {
  x>= lb & x<= ub
}

#' @export
progress <- function(x, verbose=TRUE) {
  if(verbose) {
    timestamp <- format(Sys.time(), "%H:%M:%S")
    progress.string <- sprintf("[%s] %s\n", timestamp, x)
    cat(progress.string)
  }
  invisible(NULL)
}
