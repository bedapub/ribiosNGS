#' @export ksScore
#' @useDynLib ribiosReposition, ckscore, .registration=TRUE
ksScore <- function(n, vec) {
  .Call("ckscore", as.integer(n), as.integer(vec))
}
