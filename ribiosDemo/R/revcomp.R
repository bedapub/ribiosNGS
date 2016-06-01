#' @export
makeSeq <- function(bases=c("A","T","G","C"), len=100)
  paste(sample(bases, len, replace=TRUE), collapse="")

#' @export
#' @useDynLib ribiosDemo, bios_revcomp, .registration=TRUE
revcomp <- function(x) {
  if(!is.character(x)) {
      xnames <- names(x)
      x <- as.character(x)
      names(x) <- xnames
  }
  .Call("bios_revcomp", x)
}

#' @export
revcompNative <- function(x) {
  chars <- strsplit(x,"")
  chars.rev <- lapply(chars, rev)
  dict <- c("A"="T", "G"="C", "C"="G", "T"="A", "U"="A", "N"="N")
  sapply(chars.rev, function(x) paste(dict[x], collapse=""))
}
