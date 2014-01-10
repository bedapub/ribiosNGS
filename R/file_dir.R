## directory
isDir <- function(...) {
  x <- unlist(list(...))
  na.false(file.info(x)$isdir)
}

checkDir <- function(...) {
  x <- unlist(list(...))
  all(isDir(...))
}
assertDir <- function(...) {
  haltifnot(checkDir(...),
            msg="Not all directories exist\n")
  return(invisible(TRUE))
}

## files
checkFile <- function(...) {
  x <- unlist(list(...))
  all(file.exists(x))
}
assertFile <- function(...) {
  af <- checkFile(...)
  if(af) return(invisible(TRUE))
  
  x <- unlist(list(...))
  notfound <- x[!file.exists(x)]
  haltifnot(af,
            msg=paste(paste("File not found:", notfound, sep=""),
              collapse="\n"))
}
