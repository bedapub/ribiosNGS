read_gct_matrix <- function(gct.file, keep.desc=TRUE) {
  gct.file <- checkfile(gct.file)
  mat <- .Call("c_read_gct", gct.file, NULL, keep.desc)
  class(mat) <- c("GctMatrix", "matrix")
  return(mat)
}

read_gctstr_matrix <- function(string, keep.desc=TRUE) {
  mat <- .Call("c_read_gct", NULL, string, keep.desc)
  class(mat) <- c("GctMatrix", "matrix")
  return(mat)
}
