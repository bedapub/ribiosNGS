cossim <- function(x,y, na.rm=TRUE) {
  .Call("cossim", x, y, na.rm)
}
