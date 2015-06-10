read_chip <- function(x) {
  x <- path.expand(x)
  as.data.frame(.Call("c_read_chip", as.character(x)))
}
