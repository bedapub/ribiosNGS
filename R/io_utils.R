## read data with foreign key
readFKdata <- function(file, fk,...) {
  stopifnot(file.exists(file) || missing(fk))
  data <- read.table(file,...)
  stopifnot(nrow(data) == length(fk))
  if(identical(rownames(data), fk)) {
    return(data)
  } else if (identical(as.character(data[,1L]), fk)) {
    return(data[,-1L])
  } else {
    stop("Input file does not contain any column matching the forein key (fk)");
  }
}
