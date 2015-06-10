iofile <- function(x) {
  if(!exists("DATA_DIR", inherits=TRUE)) {
    if (file.exists("./data")) {
      warning("DATA_DIR defined as ./data\n")
      assign("DATA_DIR", "./data", envir=.GlobalEnv)
    } else {
      stop("DATA_DIR not defined, or ./data does not exist \n")
    }
  }
  if(missing(x)) {
    get("DATA_DIR")
  } else {
    file.path(get("DATA_DIR"), x)
  }
}
