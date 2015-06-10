find_ampliseq <- function(dir) {
  files <- dir(dir, pattern="*.cov.xls", full.names=TRUE, recursive=TRUE)
  finfo <- file.info(files)
  isValid <- !is.na(finfo$size) & finfo$size>0
  files[isValid]
}

read_ampliseq <- function(files) {
  tbls <- lapply(files, function(x) {
    tbl <- read.table(x, sep="\t", header=TRUE)
    return(tbl[,c("attributes", "total_reads")])
  })
  uniqGenes <- unique(as.vector(sapply(tbls, function(x) x$attributes)))
  ntbls <- do.call(cbind, lapply(tbls, function(x) {
    x$total_reads[match(uniqGenes, x$attributes)]
    ## matchColumn(uniqGenes, x, "attributes")$total_reads))
  }))
  rownames(ntbls) <- gsub("GENE_ID=", "", uniqGenes)
  colnames(ntbls) <- basename(dirname(files))
  return(ntbls)
}

find_and_read_ampliseq <- function(dir) {
  files <- find_ampliseq(dir)
  read_ampliseq(files)
}
