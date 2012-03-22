pwdecode <- function(password) {
  if(!grepl("^ ", password)) {
    return(password)
  }
  num <- strsplit(password, "\\\\")[[1]][-1]
  x <- as.raw(strtoi(num, base=8L))
  .Call("pwdecode",x)
}

pwencode <- function(label="VAR", key) {
  if(missing(key))
    stop("'key' must not be missing")
  res <- system(sprintf("/apps/bi/bin/pwencode %s %s", label, key), intern=TRUE)
  browser()
}
