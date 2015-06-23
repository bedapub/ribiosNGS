## read table with foreign key
readFKtable <- function(file, fk,
                        strict.order=FALSE,...) {
  fk <- as.character(fk)
  data <- read.table(file,...)
  c0 <- as.character(rownames(data))
  hasc1 <- ncol(data)>0
  if(hasc1) c1 <- as.character(data[,1L])
  if(strict.order) {
    if(identical(c0, fk)) {
      return(data)
    } else if (hasc1 && identical(c1, fk)) {
      res <- data[,-1L,drop=FALSE]
      if(!anyDuplicated(fk))
        rownames(res) <- fk
      return(res)
    } else {
      stop("The 1st/2nd column of the input file are not stricktly identical with the foreign keys");
    }
  } else {
    if(!hasc1) {
      return(data[match(fk, c0),])
    } else {
      inc0 <- mean(fk %in% c0)
      inc1 <- mean(fk %in% c1)
      if(inc0>inc1 || (inc0==inc1 & inc0>0.5)) {
        return(data[match(fk, c0),])
      } else if (inc0<inc1) {
        res <- data[match(fk, c1),-1,drop=FALSE]
        if(!anyDuplicated(fk))
          rownames(res) <- fk
        return(res)
      } else {
        stop("The 1st/2nd column of the input file seem not to contain foreign keys");
      }
    }
  }
}
