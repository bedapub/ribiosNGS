`[.GeneSets` <- function(x,i, ...) {
  r <- NextMethod("[")
  as(r, "GeneSets")
}


matchGenes <- function(x, vec, na.rm=TRUE) {
  if(!is(x, "GeneSets"))
    stop("'x' must be a 'GeneSets' object")
  haltifnot(is.vector(vec),
            msg="'vec' must be a vector of gene names")
  xgl <- lapply(x, function(x) unique(x$genes))
  xgenes <- unlist(xgl)
  xgenes[is.na(xgenes) | xgenes=="" | xgenes=="-"] <- NA
  xgenes.ind <- match(xgenes, vec)
  xlens <- sapply(xgl, length)
  xfac <- factor(rep(1:length(xgl), xlens), levels=1:length(xgl))
  gs.indices <- split(xgenes.ind, xfac)
  gs.indices <- lapply(gs.indices, function(x) {
    if(na.rm) x <- x[!is.na(x)]
    return(x)
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

## public methods
GeneSets <- function(list, name) {
  res <- new("GeneSets",list)
  res@name <- name
  return(res)
}
readGmt <- function(file, name=basefilename(file)) {
  rl <- read_gmt_list(file)
  res <- GeneSets(rl, name=name)
  return(res)
}
readGmts <- function(..., names=NULL) {
  files.list <- list(...)
  files <- unlist(files.list, use.names=TRUE)
  fnames <- names(files)
  if(!is.null(fnames))
    names <- fnames
  if(is.null(names)) {
    names <- basefilename(files)
  } else {
    haltifnot(length(files)==length(names),
              msg="'names' must have the same length as 'files'")
  }
  res <- lapply(seq(along=files), function(i)
                readGmt(files[i], name=names[i]))
  names(res) <- names
  res <- as(res, "GeneSetsList")
  return(res)
}
parseGmt <- function(file, vec, min, max) {
  res <- read_gmt_list(file)
  res <- as(res, "GeneSets")
  ind <- matchGenes(res, vec, na.rm=TRUE)

  ## final filtering
  nmin <- ifelse(!missing(min) && is.numeric(min), min, 0)
  nmax <- ifelse(!missing(max) && is.numeric(max), max, Inf)
  ind <- ind[sapply(ind, function(x) length(x)>=nmin & length(x)<=nmax)]
  return(ind)
}
