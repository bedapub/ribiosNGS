checkfile <- function(filename) {
  if(!file.exists(filename))
    stop(filename, " does not exist\n");
  ## note that path.expand is necessary for C procedures
  return(path.expand(filename))
}
