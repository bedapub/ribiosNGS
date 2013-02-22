cossim <- function(x,y, na.rm=TRUE) {
  .Call("cossim", x, y, na.rm)
}
cosdist <- function(x,y, na.rm=TRUE) {
  1-.Call("cossim", x, y, na.rm)
}
