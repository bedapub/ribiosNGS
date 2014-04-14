readGmt <- function(file) {
  res <- read_gmt_list(file)
  as(res, "GeneSets")
}

`[.GeneSets` <- function(x,i, ...) {
  r <- NextMethod("[")
  as(r, "GeneSets")
}


matchGenes <- function(x, vec, na.rm=TRUE) {
  if(!is(x, "GeneSets"))
    stop("'x' must be a 'GeneSets' object")
  haltifnot(is.vector(vec),
            msg="'vec' must be a vector of gene names")
  gs.indices <- lapply(x, function(xx) {
    genes <- setdiff(xx$genes,c("", "-"))
    ind <- match(genes, vec)
    if(na.rm) ind <- ind[!is.na(ind)]
    return(ind)
  })
  names(gs.indices) <- gsName(x)
  return(gs.indices)
}

geneCount <- function(x,i=NULL) {
  if(!is(x, "GeneSets"))
    stop("'x' must be a 'GeneSets' object")
  res <- sapply(x, function(xx) uniqueLength(xx$genes))
  if(!missing(i)) res <- res[i]
  names(res) <- gsName(x, i)
  return(res)
}

geneCountFilter <- function(x, min, max) {
  if(!is(x, "GeneSets"))
    stop("'x' must be a 'GeneSets' object")
  gmt.count <- sapply(x, function(xx) uniqueLength(xx$genes))
  sel <- rep(TRUE, length(x))
  if(!missing(min)) sel <- sel & gmt.count >= min
  if(!missing(max)) sel <- sel & gmt.count <= max
  x[sel]
}
