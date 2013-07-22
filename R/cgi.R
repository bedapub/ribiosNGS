cgiInit <- function() {
  invisible(.Call("r_cgiInit"))
}

cgiGet2Post <- function() {
  invisible(.Call("r_cgiGet2Post"))
}

cgiGet2PostReset <- function() {
  invisible(.Call("r_cgiGet2PostReset"))
}

cgiHeader <- function(header) {
  invisible(.Call("r_cgiHeader",
                  as.character(header)))
}

cgiParams <- function() {
  return(.Call("r_cgiParameters"))
}
