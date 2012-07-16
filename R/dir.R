isDir <- function(...) {
  x <- unlist(list(...))
  na.false(file.info(x)$isdir)
}

checkDir <- isDir

assertDir <- function(...) {
  stopifnot(checkDir(...))
}


