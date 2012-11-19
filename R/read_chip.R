read_chip <- function(x) {
  x <- path.expand(x)
  .Call("read_chip", as.character(x))
}
