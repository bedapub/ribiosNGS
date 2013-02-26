options(cores=6L)
perm <- function(exp, templates, exp.dist,
                 Nrand=1000) {
  mat <- mclapply(1:ncol(exp), function(nc) {
    vec <- exp[, nc, drop=TRUE]
    rvec <- randmat(vec, nrow(templates), as.integer(Nrand))
    rand.dist <- cosdist(rvec, templates)
    dp <- empval(exp.dist[, nc], unlist(rand.dist))
  })
  res <- do.call(cbind, mat)
  rownames(res) <- colnames(templates)
  colnames(res) <- colnames(exp)
  return(res)
}

## TODO (David): Add weights options to support negative markers
## TODO (David): Make cosdist working with NA values in the matrix

ntp <- function(matrix, genesets.ind,
                row.scale=TRUE, Nrand=1000) {
  haltifnot(all(sapply(genesets.ind, function(x) is.numeric(x))),
            msg="'genesets.ind' must be a named list of integers indices of gene sets")
  
  normed.exp <- rowscale(matrix)
  genesets.uniqueInd <- unique(unlist(genesets.ind))
  nonCls <- ifelse(length(genesets.ind)==2L, -1L, 0L)
  templates <- sapply(genesets.ind,
                      function(x) ifelse(genesets.uniqueInd %in% x, 1L, nonCls))
  colnames(templates) <- names(genesets.ind)
  rownames(templates) <- genesets.uniqueInd

  gct.template.dist <- cosdist(templates,
                               normed.exp[genesets.uniqueInd,])
  gct.template.dist.p <- perm(normed.exp, templates,
                              gct.template.dist, Nrand=Nrand)
  gct.pred.ind <- apply(gct.template.dist, 2L, function(x) which(x==min(x)))
  gct.pred <- sapply(1:ncol(gct.template.dist), function(x) rownames(gct.template.dist)[gct.pred.ind[x]])
  gct.pred.p <- sapply(1:ncol(gct.template.dist),
                       function(x) gct.template.dist.p[gct.pred.ind[x], x])
  res <- list(prediction=gct.pred,
              predPval=gct.pred.p,
              distMat=gct.template.dist,
              pValMat=gct.template.dist.p)
  return(res)
}
