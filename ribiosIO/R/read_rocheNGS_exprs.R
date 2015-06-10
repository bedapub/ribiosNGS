## read roche NGS pipeline (built mainly by Fabian Birzele) expression files
read_rocheNGS_exprs <- function(filename) {
  filename <- checkfile(filename)
  res <- .Call("c_read_rocheNGS_exprs", filename)
  return(res)
}
