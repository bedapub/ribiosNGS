## divide and conquer strategy for the ISA algorithm, and merge the results into one ISAModule object
phenoApply <- function(eSet, pFactor, FUN,...) {
  stopifnot(length(pFactor) == dim(eSet)[2])
  FUN.dots <- list(...)
  if(!is.factor(pFactor))
    pFactor <- factor(pFactor)
  res <- tapply(1:length(pFactor),
                pFactor,
                function(i) {
                  fun.list <- c(list(FUN, quote(eSet[,i])), FUN.dots)
                  eval(as.call(fun.list))
                })
  names(res) <- levels(pFactor)
  return(res)
}

dcISA <- function(data,
                  group,
                  globalflist = filterfun(function(x) IQR(x)>0.5),
                  flist=NA,
                  uniqueEntrez=TRUE,
                  thr.gene = seq(2, 4, by=0.5),
                  thr.cond = seq(1, 3, by=0.5),
                  no.seeds = 100) {
  isa2:::isa.status("ISA on an ExpressionSet", "in")
  if (!is(data, "ExpressionSet")) {
    stop("Please supply an ExpressionSet object")
  }
  stopifnot(length(group) == dim(data)[2])
  if(!is.factor(group))
    group <- factor(group)
  if (is.function(globalflist) && require(genefilter)) {
    selected <- genefilter(data, globalflist)
    data <- data[selected, ]
  }
  else if (!is.na(globalflist)) {
    stop("Could not interpret `globalflist' argument, should be a function or `NA'")
  }
  
  isa.dc <- phenoApply(data,
                       group,
                       ISA,
                       flist=flist,
                       uniqueEntrez=uniqueEntrez,
                       thr.gene=thr.gene,
                       thr.cond=thr.cond,
                       no.seeds=no.seeds)
  stopifnot(length(isa.dc)==nlevels(group))
  module.group <- rep(levels(group), sapply(isa.dc, length))
  module.group <- factor(module.group,
                         levels=levels(group))
  
  ## seed data
  seed.data <- lapply(isa.dc, seedData)
  comb.seed.data <- do.call(rbind, seed.data)
  rownames(seed.data) <- NULL
  
  ## the run data remains the samediseases.prior.dc.
  comb.run.data <- runData(isa.dc[[1]])
  
  ## both genes and conditions: 0 has to be filled
  fill.zero <- function(list) {
    unique.rownames <- unique(unname(unlist(sapply(list, function(x) rownames(x)))))
    comb.ncol <- sum(sapply(list, function(x) ncol(x)))
    comb.matrix <- matrix(data=0, nrow=length(unique.rownames), ncol=comb.ncol,
                          dimnames=list(unique.rownames, NULL))
    ## slow: iterate the list and fill the matrix
    ind <- 1
    for(i in seq(along=list)) {
      curr.item.length <- ncol(list[[i]])
      if(curr.item.length>0) {
        curr.item <- list[[i]]
        curr.match <- match(row.names(curr.item), unique.rownames)
        comb.matrix[curr.match, ind:(ind+curr.item.length-1)] <- curr.item
      }
      ind <- ind+curr.item.length
    }
    return(comb.matrix)
  }
  
  comb.gene.data <- fill.zero(lapply(isa.dc, function(x) x@genes))
  comb.conditions.data <- fill.zero(lapply(isa.dc, function(x) x@conditions))
  
  ## pData
  comb.conditions.names <- rownames(comb.conditions.data)
  data.pData <- pData(data)
  comb.conditions.names.match <- match(comb.conditions.names,
                                       rownames(data.pData))
  comb.pData <- data.pData[comb.conditions.names.match,]

  ## check groups
  stopifnot(length(module.group)==dim(comb.gene.data)[2])
  stopifnot(length(module.group)==dim(comb.conditions.data)[2])
  
  comb.isa <- new("dcISAModules",
                  group=module.group,
                  genes=comb.gene.data,
                  conditions=comb.conditions.data,
                  rundata=comb.run.data,
                  seeddata=comb.seed.data)
  pData(comb.isa) <- comb.pData
  return(comb.isa)
}
#### for debugging
## assignInNamespace("ISA.divide.and.conquer",ISA.divide.and.conquer, "ribios")
