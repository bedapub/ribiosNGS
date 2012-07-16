checkFile <- function(...) {
  x <- unlist(list(...))
  all(file.exists(x))
}
assertFile <- function(...) {
  stopifnot(checkFile(...))
}
