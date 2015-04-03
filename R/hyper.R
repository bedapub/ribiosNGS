gmthyper <- function(genes, background, gmtlist) {
  genes <- unique(genes)
  background <- unique(background)

  stopifnot(all(genes %in% background))
  gsgenes <- lapply(gsGenes(gmtlist), intersect, background)

  k <- length(genes)
  m <- sapply(gsgenes, length)
  nn <- length(background)-m
  q <- sapply(gsgenes, function(x) sum(genes %in% x))

  pover <- 1-phyper(q-1, m, nn, k, lower.tail=TRUE) 
  pAdj <- p.adjust(pover, "fdr")
  res <- data.frame(gs=gsName(gmtlist),
                    k=k,
                    m=m,
                    n=nn,
                    q=q,
                    p=pover,
                    pAdj=pAdj,
                    row.names=NULL)
}
gmthyperList <- function(gene.list, background, gmtlist) {
  names <- names(gene.list)
  if(is.null(names))
    stop("gene.list must have names")
  res.list <- lapply(gene.list, function(genes)
                     gmthyper(genes, babel.between.bgGenes, gmtlist))
  res <- do.call(rbind, res.list)
  res$genelist <- rep(names, sapply(res.list, nrow))
  return(res)
}
