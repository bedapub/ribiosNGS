isGctFile <- function(file) {
  headers <- readLines(file, n=3)
  isFL <- headers[[1]]=="#1.2"
  isSL <- grepl("^[0-9]*\\s[0-9]*$", headers[[2]])
  isTL <- grepl("^NAME\\sDescription\\s.*", headers[[3]], ignore.case=TRUE)
  return(isFL && isSL && isTL)
}
