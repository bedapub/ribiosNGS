write_gmt <- function(gmt, file) {
  invisible(.Call("write_gmt", gmt, as.character(file)))
}
