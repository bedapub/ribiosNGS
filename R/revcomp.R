makeSeq <- function(bases=c("A","T","G","C"), len=100)
  paste(sample(bases, len, replace=TRUE), collapse="")


revcomp <- function(x) {
  .Call("bios_revcomp", as.character(x))
}

revcompNative <- function(x) {
  chars <- strsplit(x,"")
  chars.rev <- lapply(chars, rev)
  dict <- c("A"="T", "G"="C", "C"="G", "T"="A", "U"="T")
  sapply(chars.rev, function(x) paste(dict[x], collapse=""))
}
