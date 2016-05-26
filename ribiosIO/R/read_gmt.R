read_gmt_list <- function(gmt.file) {
  gmt.file <- checkfile(gmt.file)
  ll <- .Call("c_read_gmt", gmt.file)
  return(ll)
}
