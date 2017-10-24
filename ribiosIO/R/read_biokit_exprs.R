#' Read BioKit expression file into a data.frame
#' 
#' @param filename A BioKit expression file
#' 
#' The function uses an efficient C routine to read BioKit expression files.
#' 
#' @example 
#' 
read_biokit_exprs <- function(filename) {
  filename <- checkfile(filename)
  res <- .Call("c_read_biokit_exprs", filename)
  return(res)
}
