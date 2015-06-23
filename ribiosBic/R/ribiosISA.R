## a ribios implementation of ISA
library(multicore)

ribios.isa.mc <- function (data,
                           thr.row = seq(2, 4, by = 0.5),
                           thr.col = seq(1, 3, by = 0.5),
                           no.seeds = 100,
                           direction = c("updown", "updown"),
                           sparsity = c(1,2,5,25,125),
                           mc.cores=1L) 
{
    isa2:::isa.status("Performing complete ISA work flow", "in")
    if (!is.matrix(data)) {
        stop("`data must be a matrix")
    }
    mcflag <- ifelse(require(multicore), 1, 0)
    normed.data <- isa.normalize(data)
    row.seeds <- generate.seeds(length = nrow(data), count = no.seeds,
                                sparsity=sparsity)
    thr.list <- expand.grid(thr.row = thr.row, thr.col = thr.col)
    thr.list <- unlist(apply(thr.list, 1, list), recursive = FALSE)
    if(mcflag) {
      isaresults <- mclapply(thr.list, function(x) isa.iterate(normed.data, 
                                                               row.seeds = row.seeds,
                                                               thr.row = x["thr.row"], thr.col = x["thr.col"], 
                                                               direction = direction),
                             mc.cores=mc.cores)
      isaresults <- mclapply(isaresults, function(x) isa.unique(normed.data, x),
                             mc.cores=mc.cores)
      isaresults <- mclapply(isaresults, function(x) isa.filter.robust(data = data, 
                                                                       normed.data = normed.data,
                                                                       isares = x,
                                                                       row.seeds = row.seeds),
                             mc.cores=mc.cores)
    } else {
      isaresults <- lapply(thr.list, function(x) isa.iterate(normed.data, 
                                                             row.seeds = row.seeds, thr.row = x["thr.row"], thr.col = x["thr.col"], 
                                                             direction = direction))
      isaresults <- lapply(isaresults, function(x) isa.unique(normed.data, 
                                                              x))
      isaresults <- lapply(isaresults, function(x) isa.filter.robust(data = data, 
                                                                     normed.data = normed.data,
                                                                     isares = x,
                                                                     row.seeds = row.seeds))
    }
    result <- list()
    result$rows <- do.call(cbind, lapply(isaresults, "[[", "rows"))
    result$columns <- do.call(cbind, lapply(isaresults, "[[", 
        "columns"))
    result$seeddata <- do.call(rbind, lapply(isaresults, "[[", 
        "seeddata"))
    result$rundata <- isaresults[[1]]$rundata
    result$rundata$N <- sum(sapply(isaresults, function(x) x$rundata$N))
    result <- isa.unique(normed.data, result)
    isa2:::isa.status("DONE", "out")
    result
}
## couterpart of eisa:::isa.result.to.ISAModules
ribios.isa.result.to.ISAModules <- function(modules, data) {
  new.modules <- new("ISAModules",
                     genes = modules$rows,
                     conditions = modules$columns, 
                     seeddata = modules$seeddata, rundata = modules$rundata)
  new.modules@rundata$annotation <- annotation(data)
  new.modules@rundata$pData <- pData(data)
  rownames(new.modules@genes) <- featureNames(data)
  rownames(new.modules@conditions) <- sampleNames(data)
  new.modules
}

## perms: 1 is enough (multiple permutation does not really increase the power)
## when possible, no.seeds could be set higher
ribiosISA <- function(data,
                      thr.gene=seq(2,4,by=0.5),
                      thr.cond=seq(1,3,by=0.5),
                      sparsity = c(1, 2, 5, 25, 100),
                      no.seeds=300,
                      mc.cores=min(length(thr.gene)*length(thr.cond), 6L)) {
  isa2:::isa.status("ISA on an ExpressionSet", "in")
  if (!is(data, "ExpressionSet")) {
    stop("Please supply an ExpressionSet object")
  }
  ## ribiosExpression::keepHighestVarProbe does the job of 'uniqueEntrez'
  modules <- ribios.isa.mc(exprs(data), thr.row = thr.gene, thr.col = thr.cond, 
                           no.seeds = no.seeds, sparsity=sparsity, 
                           mc.cores=mc.cores)
  modules <- ribios.isa.result.to.ISAModules(modules, data)
  isa2:::isa.status("DONE", "out")
  modules
}
