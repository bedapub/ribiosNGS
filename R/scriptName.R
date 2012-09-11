scriptName <- function() {
  filename <- grep("--file=", commandArgs(), val=TRUE)
  lf <- length(filename)
  if(lf>1) {
    warning("Multiple --file options found, only using the first")
    filename <- filename[1L]
  } else if (lf==1) {
    restname <- strsplit(filename, "=")[[1L]][-1L]
    return(paste(restname, collapse="="))
  } else if (lf==0) { ## no --file found
    filename <- getArg("f", onlyArg=NULL, missingArg=NULL)
    return(filename) ## if missing: NULL, otherwise: the file name
  }
}
