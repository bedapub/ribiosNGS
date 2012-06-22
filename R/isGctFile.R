isGctFile <- function(file) {
  headers <- readLines(file, n=3)
  isFL <- grepl("^#1.2", headers[[1]])
  isSL <- grepl("^[0-9]*\\s[0-9]*\\s*$", headers[[2]])
  isTL <- grepl("^NAME\\sDescription\\s.*", headers[[3]], ignore.case=TRUE)
  return(isFL && isSL && isTL)
}
