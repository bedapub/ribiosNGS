read_gct_matrix <- function(gct.file, keep.desc=TRUE) {
  if(!file.exists(gct.file)) {
    stop(gct.file, " does not exist\n")
  }
  ## file expansion cannot be done by ls_createFromFile
  gct.file <- path.expand(gct.file)
  mat <- .Call("read_gct", gct.file, keep.desc)
  return(mat)
}
