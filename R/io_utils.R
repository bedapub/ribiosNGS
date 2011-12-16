## read table with foreign key
readFKtable <- function(file, fk,...) {
  stopifnot(file.exists(file) || missing(fk))
  data <- read.table(file,...)
  stopifnot(nrow(data) == length(fk))
  fk <- as.character(fk)
  if(identical(rownames(data), fk)) {
    return(data)
  } else if (identical(as.character(data[,1L]), fk)) {
    res <- data[,-1L,drop=FALSE]
    if(!anyDuplicated(fk))
      rownames(res) <- fk
    return(res)
  } else {
    stop("Input file does not contain any column matching the forein key (fk)");
  }
}
