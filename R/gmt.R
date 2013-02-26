readGmt <- function(file) {
  res <- read_gmt_list(file)
  class(res) <- "GeneSets"
  res
}

`[.GeneSets` <- function(x,i, ...) {
  r <- NextMethod("[")
  class(r) <- "GeneSets"
  r
}
print.GeneSets <- function(x,...) {
  cat("[[", length(x), "GeneSets ]]", "\n")
  heads <- 1:pmin(3L, length(x))
  cat("--------------------\n")
  for(i in heads) {
    cat("name:", x[[i]]$name, "\n")
    cat("description:", x[[i]]$description, "\n")
    cat("genes: ", paste(head(x[[i]]$genes), collapse=","), ",...", "\n", sep="")
    cat("--------------------\n")
  }
  cat("...\n")
}
gsNames <- function(x,i) UseMethod("gsNames")
gsNames.GeneSets <- function(x, i) {
  res <- sapply(x, function(x) x$name)
  if(!missing(i)) res <- res[i]
  return(res)
}
gsDescs <- function(x,i) UseMethod("gsDescs")
gsDescs.GeneSets <- function(x, i) {
  res <- sapply(x, function(x) x$description)
  if(!missing(i)) res <- res[i]
  names(res) <- gsNames(x, i)
  return(res)
}
gsGenes <- function(x,i) UseMethod("gsGenes")
gsGenes.GeneSets <- function(x, i) {
  res <- lapply(x, function(x) x$gene)
  names(res) <- gsNames(x)
  if(!missing(i)) {
    res <- res[i]
    if(length(i)==1) res <- res[[1L]]
  }
  return(res)
}

matchGenes <- function(x,vec, na.rm=TRUE) UseMethod("matchGenes")
matchGenes.GeneSets <- function(x, vec, na.rm=TRUE) {
  haltifnot(is.vector(vec),
            msg="'vec' must be a vector of gene names")
  gs.indices <- lapply(x, function(xx) {
    genes <- setdiff(xx$genes,c("", "-"))
    ind <- match(genes, vec)
    if(na.rm) ind <- ind[!is.na(ind)]
    return(ind)
  })
  names(gs.indices) <- gsNames(x)
  return(gs.indices)
}

geneCount <- function(x,i) UseMethod("geneCount")
geneCount.GeneSets <- function(x,i) {
  res <- sapply(x, function(xx) uniqueLength(xx$genes))
  if(!missing(i)) res <- res[i]
  names(res) <- gsNames(x, i)
  return(res)
}

geneCountFilter <- function(x,min, max) UseMethod("geneCountFilter")
geneCountFilter.GeneSets <- function(x, min, max) {
  gmt.count <- sapply(x, function(xx) uniqueLength(xx$genes))
  sel <- rep(TRUE, length(x))
  if(!missing(min)) sel <- sel & gmt.count >= min
  if(!missing(max)) sel <- sel & gmt.count <= max
  x[sel]
}


##----------------------------------------##
## obsolete functions
##----------------------------------------##

filterGmtByGeneCount <- function(gmtlist,min=5L, max=1000L) {
  .Deprecated("geneCountFilter", package="ribiosGSEA")
  gmt.count <- sapply(gmtlist, function(x) uniqueLength(x$genes))
  gmt.keep <- gmt.count >= min & gmt.count <= max
  gmtlist[gmt.keep]
}

gmtIndices <- function(gmtlist, symbols) {
  .Deprecated("matchGenes", package="ribiosGSEA")
  gs.names <- sapply(gmtlist, function(x) x$name)
  gs.genes <- lapply(gmtlist, function(x) setdiff(x$genes,c("", "-")))
  gs.indices <- lapply(gs.genes, function(x) which(symbols %in% x))
  names(gs.indices) <- gs.names
  gs.indices
}
