read_pheno <- function(file) {
  lns <- readLines(file)
  txt <- textConnection(paste(lns, collapse="\n"))
  if(length(lns)==3 && grepl("^#", lns[2])) {
    sclass <- read_cls(txt)
    tbl <- data.frame(Array=seq(along=sclass),
                      Class=sclass)
  } else {
    tbl <- read.csv(txt, sep="\t", header=TRUE, comment.char="#")
  }
  close(txt)
  return(tbl)
}

read_pheno_factor <- function(file) {
  lns <- readLines(file)
  txt <- textConnection(paste(lns, collapse="\n"))
  if(length(lns)==3 && grepl("^#", lns[2])) {
    sclass <- read_cls(txt)
  } else {
    tbl <- read.csv(txt, sep="\t", header=TRUE, comment.char="#")
    if(ncol(tbl)==1) {
      sclass <- factor(tbl[,1L], levels=unique(tbl[,1L]))
    } else {
      isCov <- apply(tbl, 2L, function(x) length(unique(x)) != 1 && length(unique(x)) != nrow(tbl))
      subtbl <- tbl[,isCov,drop=FALSE]
      classes <- apply(subtbl, 1L, paste, collapse="_")
      sclass <- factor(classes, levels=unique(classes))
    }
  }
  close(txt)
  return(sclass)
}
