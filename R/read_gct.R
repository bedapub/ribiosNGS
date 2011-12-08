read_gct_matrix <- function(gct.file, keep.desc=TRUE) {
  gct.file <- checkfile(gct.file)
  mat <- .Call("read_gct", gct.file, keep.desc)
  return(mat)
}
