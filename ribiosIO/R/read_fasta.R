read_fasta <- function(file) {
  lines <- readLines(file)
  ll <- length(lines)
  iname <- grep("^>", lines)
  seqnames <- lines[iname]

  seqstarts <- iname + 1L
  seqends <- c(iname[-1]-1L, ll)
  seqs <- sapply(1:length(iname), function(x) {
    paste(lines[seq(seqstarts[x], seqends[x], 1L)], collapse="")
  })
  names(seqs) <- gsub("^>", "", seqnames)
  return(seqs)
}

write_fasta <- function(x, file) {
  out <- paste(paste(">",names(x), sep=""),
               x, sep="\n")
  writeLines(out, con=file)
}
