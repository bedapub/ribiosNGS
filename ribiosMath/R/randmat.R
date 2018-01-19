randmat <- function(vec, size, N) {
  .Call(C_randmat, vec, size, N)
}
