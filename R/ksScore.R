ksScore <- function(n, vec) {
  .Call("ckscore", as.integer(n), as.integer(vec))
}
