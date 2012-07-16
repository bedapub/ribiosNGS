isDir <- function(...) {
  x <- unlist(list(...))
  na.false(file.info(x)$isdir)
}

checkDir <- function(...) {
  x <- unlist(list(...))
  all(isDir(...))
}
assertDir <- function(...) {
  stopifnot(checkDir(...))
}

checkFile <- function(...) {
  x <- unlist(list(...))
  all(file.exists(x))
}
assertFile <- function(...) {
  stopifnot(checkFile(...))
}
