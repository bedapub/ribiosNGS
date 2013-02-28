read_pheno <- function(file) {
  lns <- readLines(file)
  txt <- paste(lns, collapse="\n")
  if(length(lns)==3 && grepl("^#", lns[2])) {
    sclass <- read_cls(textConnection(txt))
    tbl <- data.frame(Array=seq(along=sclass),
                      Class=sclass)
  } else {
    tbl <- read.csv(textConnection(txt), sep="\t", header=TRUE)
  }
  return(tbl)
}
