RIBIOS_SEQ_FILES <- NULL

getSeqFiles <- function() {
  return(getFromNamespace("RIBIOS_SEQ_FILES", ns="ribiosQC"))
}
setSeqFiles <- function(seqfiles) {
  assignInNamespace("RIBIOS_SEQ_FILES", seqfiles, ns="ribiosQC")
}
clearSeqFiles <- function() {
  assignInNamepsace("RIBIOS_SEQ_FILES", NULL, ns="ribiosQC")
}
registerSeqFile <- function(filename) {
  seqs <- getSeqFiles()
  fullname <- file.path(dirname(filename),
                        sprintf("%03d-%s",
                                length(seqs)+1L,
                                basename(filename)))
  seqs <- append(seqs, fullname)
  setSeqFiles(seqs)
  return(fullname)
}
