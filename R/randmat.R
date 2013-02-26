randmat <- function(vec, size, N) {
  .Call("randmat", vec, size, N)
}
