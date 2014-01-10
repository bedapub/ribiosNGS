#### extname alternative implementation
#### gsub(".*\\.(\\w*)", "\\1", basename(as.character(x)))
#### Not good: fail when file does not have extname

basefilename <- function(x, lower.case=FALSE) {
  if (!all(is.character(x))) 
    x <- as.character(x)
  sapply(strsplit(basename(x), "\\."), function(f) {
    res <- ifelse(length(f) == 1, f, paste(f[-length(f)],collapse="."))
    if(lower.case) res <- tolower(res)
    return(res)
  })
}

extname <- function(x, ifnotfound=NA, lower.case=FALSE) {
  ## NOT GOOD: without extension names will return the whole name
  ## gsub(".*\\.(\\w*)", "\\1", basename(as.character(x)))
  if(!all(is.character(x))) x <- as.character(x)
  sapply(strsplit(basename(x), "\\."), function(f) {
    res <- ifelse(length(f)==1, ifnotfound, f[length(f)])
    if(lower.case) res <- tolower(res)
    return(res)
  })
}
