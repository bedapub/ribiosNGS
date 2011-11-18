iofile <- function(x) {
  if(!exists("DATA_DIR", inherits=TRUE)) {
    if (file.exists("./data")) {
      warning("DATA_DIR defined as ./data\n")
      DATA_DIR <<- "./data"
    } else {
      stop("DATA_DIR not defined, or ./data does not exist \n")
    }
  }
  if(missing(x)) {
    DATA_DIR
  } else {
    file.path(DATA_DIR, x)
  }
}
