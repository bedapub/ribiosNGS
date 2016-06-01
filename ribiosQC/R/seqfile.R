RIBIOS_SEQ_FILES <- NULL

#' @export getSeqFiles
getSeqFiles <- function() {
  return(getFromNamespace("RIBIOS_SEQ_FILES", ns="ribiosQC"))
}
#' @export setSeqFiles
setSeqFiles <- function(seqfiles) {
  assignInNamespace("RIBIOS_SEQ_FILES", seqfiles, ns="ribiosQC")
}
#' @export clearSeqFiles
clearSeqFiles <- function() {
  assignInNamepsace("RIBIOS_SEQ_FILES", NULL, ns="ribiosQC")
}
#' @export registerSeqFile
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
