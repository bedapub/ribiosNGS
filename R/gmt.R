filterGmtByGeneCount <- function(gmtlist,min=5L, max=1000L) {
  gmt.count <- sapply(gmtlist, function(x) uniqueLength(x$genes))
  gmt.keep <- gmt.count >= min & gmt.count <= max
  gmtlist[gmt.keep]
}

gmtIndices <- function(gmtlist, symbols) {
  gs.names <- sapply(gmtlist, function(x) x$name)
  gs.genes <- lapply(gmtlist, function(x) setdiff(x$genes,c("", "-")))
  gs.indices <- lapply(gs.genes, function(x) which(symbols %in% x))
  names(gs.indices) <- gs.names
  gs.indices
}
