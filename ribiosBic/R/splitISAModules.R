## split modules by correlations
splitModuleByCor <- function(...) {
  .Deprecated("splitISAmodule")
}

splitMatCol <- function(matrix) {
  res <- lapply(1:ncol(matrix), function(i) {
    x <- matrix[,i]
    if(all(x>=0) | all(x<=0)) {
      return(list(x))
    } else {
      x.pos <- x; x.pos[x<0] <- 0
      x.neg <- x; x.neg[x>0] <- 0
      return(list(x.pos, x.neg))
    }
  })
  return(res)
}

splitISAModules <- function(modules, module,
                            splitFeatures=TRUE, splitConditions=FALSE) {

  if(missing(module))
    module <- 1:ncol(modules@genes)

  if(!splitFeatures & !splitConditions) {
    return(modules[[module]])
  }
  
  genes <- modules@genes[,module,drop=FALSE]
  conds <- modules@conditions[,module, drop=FALSE]

  if(splitFeatures) {
    exp.genes <- splitMatCol(genes)
    exp.genes.no <- sapply(exp.genes, length)
  } else {
    exp.genes <- lapply(1:ncol(genes), function(x) list(genes[,x]))
    exp.genes.no <- rep(1L, ncol(genes))
  }
  
  if(splitConditions) {
    exp.conds <- splitMatCol(conds)
    exp.conds.no <- sapply(exp.conds, length)
  } else {
    exp.conds <- lapply(1:ncol(conds), function(x) list(conds[,x]))
    exp.conds.no <- rep(1L, ncol(conds))
  }

  slots <- rep(1L, length(module))
  slots <- slots * exp.genes.no * exp.conds.no

  expanded.genes.tmp <- lapply(seq(along=exp.conds.no), function(x)
                               rep(exp.genes[[x]], exp.conds.no[x])
                           )
  expanded.conds.tmp <- lapply(seq(along=exp.genes.no), function(x)
                               rep(exp.conds[[x]], each=exp.genes.no[x])
                               )
  expanded.genes <- do.call(cbind, unlist(expanded.genes.tmp, recursive=FALSE))
  expanded.conds <- do.call(cbind, unlist(expanded.conds.tmp, recursive=FALSE))
  
  ## fill object slots
  exp.seeddata <- do.call(rbind,lapply(seq(along=module), function(x) {
    do.call(rbind,lapply(1:slots[x], function(void) modules@seeddata[module[x],]))
  }))

  res <- modules
  res@seeddata <- exp.seeddata
  res@genes <- expanded.genes
  res@conditions <- expanded.conds
  
  return(res)
  
}
