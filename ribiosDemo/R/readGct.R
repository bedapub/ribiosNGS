#' @importFrom utils read.table
#' @importFrom stats rnorm


#' @export 
makeTempGct <- function(nrow=100, ncol=100) {
  tf <- tempfile()
  writeLines(makeGctLines(nrow=nrow, ncol=ncol), con=tf)
  tf
}

#' @export
makeGctLines <- function(nrow=100, ncol=100) {

  cnames <- 1:ncol
  title <- paste("NAME",
                 "Description",
                 paste(cnames, collapse="\t"),
                 sep="\t")
  mat <- matrix(rnorm(nrow*ncol), nrow=nrow, ncol=ncol)
  tbl <- cbind(1:nrow,
               1:nrow,
               mat)
  tbltxt <- apply(tbl, 1, function(x) paste(x, collapse="\t"))
  c("#1.2",
    paste(nrow, ncol, sep="\t"),
    title,
    tbltxt)
}

#' @export
checkfile <- function(filename) {
  if(!file.exists(filename))
    stop(filename, " does not exist\n");
  ## note that path.expand is necessary for C procedures
  return(path.expand(filename))
}

#' @export
#' @useDynLib ribiosDemo, bios_readgct, .registration=TRUE
readGct <- function(gct.file, keep.desc=TRUE) {
  gct.file <- checkfile(gct.file)
  mat <- .Call("bios_readgct", gct.file, keep.desc)
  return(mat)
}

#' @export
readGctNative <- function(gct.file, keep.desc=TRUE) {
  tbl <- read.table(gct.file, skip=2L, header=TRUE, check.names=FALSE)
  mat <- data.matrix(tbl[,-c(1:2)])
  rownames(mat) <- tbl[,1L]
  
  check <- as.integer(strsplit(readLines(gct.file, n=2L)[2], "\t")[[1]])
  stopifnot(nrow(mat)==check[1] && ncol(mat)==check[2])
  
  if(keep.desc)
    attr(mat, "desc") <- as.character(tbl[,2L])

  return(mat)
}
