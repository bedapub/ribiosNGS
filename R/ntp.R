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

ntpTemplates <- function(genesets, featureNames) {
  genesets.ind <- matchGenes(genesets, featureNames)
  templates <- list2mat(genesets.ind)
  class(templates) <- c("ntpTemplates", "matrix")
  return(templates)
}
ntpBiTemplates <- function(genesetsPos, genesetsNeg, featureNames) {
  haltifnot(length(genesetsPos)==length(genesetsNeg),
            msg="Positive and negative gene sets must be of the same length")
  gsPos.ind <- matchGenes(genesetsPos, featureNames)
  gsNeg.ind <- matchGenes(genesetsNeg, featureNames)
  gs.uind <- unique(c(unlist(gsPos.ind),
                      unlist(gsNeg.ind)))
  templates <- sapply(seq(along=gsPos.ind),
                      function(x) {
                        res <- rep(0L, length=length(gs.uind))
                        res[gs.uind %in% gsPos.ind[[x]]] <- 1L
                        res[gs.uind %in% gsNeg.ind[[x]]] <- -1L
                        return(res)
                      })
  classes <- gsNames(genesetsPos)
  colnames(templates) <- classes
  rownames(templates) <- gs.uind
  class(templates) <- c("ntpTemplates", "matrix")
  return(templates)
}

print.ntpTemplates <- function(x,verbose=TRUE, ...) {
  cat("A NTP-template of", length(x$index), "genes",
      "and", length(x$classes), "classes\n")
  if(verbose) NextMethod("print")
}

## TODO (David): Make cosdist working with NA values in the matrix
ntp <- function(matrix, ntpTemplates,
                row.scale=TRUE, Nrand=1000) {
  haltifnot(is(ntpTemplates, "ntpTemplates"),
            msg="'templates' must be a ntpTemplates. Use function 'ntpTemplates' or 'ntpBiTemplates' to produce such an object")

  if(row.scale) matrix <- rowscale(matrix)

  genesets.uniqueInd <- as.integer(rownames(ntpTemplates))
  classes <- colnames(ntpTemplates)
  templates <- ntpTemplates

  gct.template.dist <- cosdist(templates,
                               matrix[genesets.uniqueInd,])
  gct.template.dist.p <- perm(matrix, templates,
                              gct.template.dist, Nrand=Nrand)
  gct.pred.ind <- apply(gct.template.dist, 2L, function(x) which(x==min(x)))
  gct.pred <- sapply(1:ncol(gct.template.dist), function(x) classes[gct.pred.ind[x]])
  gct.pred.p <- sapply(1:ncol(gct.template.dist),
                       function(x) gct.template.dist.p[gct.pred.ind[x], x])
  res <- list(prediction=gct.pred,
              predPval=gct.pred.p,
              distMat=gct.template.dist,
              pValMat=gct.template.dist.p)
  return(res)
}
