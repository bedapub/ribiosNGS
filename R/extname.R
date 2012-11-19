#### extname alternative implementation
#### gsub(".*\\.(\\w*)", "\\1", basename(as.character(x)))
#### Not good: fail when file does not have extname

basefilename <- function(x) {
  if (!all(is.character(x))) 
    x <- as.character(x)
  sapply(strsplit(basename(x), "\\."), function(f) {
    ifelse(length(f) == 1, f, paste(f[-length(f)],collapse="."))
  })
}

extname <- function(x, ifnotfound=NA) {
  ## NOT GOOD: without extension names will return the whole name
  ## gsub(".*\\.(\\w*)", "\\1", basename(as.character(x)))
  if(!all(is.character(x))) x <- as.character(x)
  sapply(strsplit(basename(x), "\\."), function(f) {
    ifelse(length(f)==1, ifnotfound, f[length(f)])
  })
}
