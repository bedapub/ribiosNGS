repA <- function(x) {
  .Call("bios_repA", as.integer(x))
}

revcomp <- function(x) {
  .Call("bios_revcomp", x)
}

getPi <- function(x=1) {
  stopifnot(storage.mode(x) %in% c("logical", "integer", "double", "numeric"))
  x <- as.numeric(x)
  .Call("c_getPi", x)
}

printInt <- function(x=0) {
  stopifnot(storage.mode(x) %in% c("logical", "integer", "double", "numeric"))
  x <- as.integer(x)
  invisible(.Call("c_printInt", x))
}
