## condISA (wrapper) and cond.isa: ISA algorithm that starts with random seeds of samples
cond.isa <- function(data,
                      thr.row = seq(1,3,by = 0.5), 
                      thr.col = seq(1,3,by = 0.5),
                      no.seeds = 100, 
                      col.seeds,
                      direction = c("updown", "updown")) {
    isa2:::isa.status("Performing complete ISA work flow", "in")
    if (!is.matrix(data)) {
        stop("`data must be a matrix")
    }
    normed.data <- isa2:::isa.normalize(data)
    if(missing(col.seeds)) {
      col.seeds <- generate.seeds(length = ncol(data), count = no.seeds)
    }
    if(dim(col.seeds)[1] != dim(data)[2]) {
      stop("'col.seeds' must be of the same length as the sample number!\n")
    }
    thr.list <- expand.grid(thr.row = thr.row, thr.col = thr.col)
    thr.list <- unlist(apply(thr.list, 1, list), rec = FALSE)
    isaresults <- lapply(thr.list, function(x) isa2:::isa.iterate(normed.data, 
        col.seeds = col.seeds, thr.row = x["thr.row"], thr.col = x["thr.col"], 
        direction = direction))
    isaresults <- lapply(isaresults, function(x) isa2:::isa.unique(normed.data, 
        x))
    isaresults <- lapply(isaresults, function(x) isa2:::isa.filter.robust(data = data, 
        normed.data = normed.data, isares = x, col.seeds = col.seeds))
    result <- list()
    result$rows <- do.call(cbind, lapply(isaresults, "[[", "rows"))
    result$columns <- do.call(cbind, lapply(isaresults, "[[", 
        "columns"))
    result$seeddata <- do.call(rbind, lapply(isaresults, "[[", 
        "seeddata"))
    result$rundata <- isaresults[[1]]$rundata
    result$rundata$N <- sum(sapply(isaresults, function(x) x$rundata$N))
    result <- isa2:::isa.unique(normed.data, result)
    isa2:::isa.status("DONE", "out")
    result
}
condISA <- function (data, flist = filterfun(function(x) IQR(x) > 0.5), 
                      uniqueEntrez = TRUE, 
                      thr.gene = seq(2, 4, by = 0.5), col.seeds=NULL,
                      thr.cond = seq(1,3, by = 0.5), no.seeds = 100) 
{
    isa2:::isa.status("ISA on an ExpressionSet", "in")
    if (!is(data, "ExpressionSet")) {
        stop("Please supply an ExpressionSet object")
    }
    if (is.function(flist)) {
        library(genefilter)
        selected <- genefilter(data, flist)
        data <- data[selected, ]
    }
    else if (!is.na(flist)) {
        stop("Could not interpret `flist' argument, should be a function or `NA'")
    }
    if (uniqueEntrez) {
        library(paste(sep = "", annotation(data), ".db"), character.only = TRUE)
        ENTREZ <- get(paste(sep = "", annotation(data), "ENTREZID"))
        entrez <- mget(featureNames(data), ENTREZ)
        keep <- sapply(entrez, function(x) length(x) > 1 || !is.na(x))
        data <- data[keep, ]
        vari <- apply(exprs(data), 1, var)
        larg <- findLargest(featureNames(data), vari, data = annotation(data))
        data <- data[larg, ]
    }
    modules <- cond.isa(exprs(data), thr.row = thr.gene, thr.col = thr.cond, 
        col.seeds=col.seeds)
    modules <- eisa:::isa.result.to.ISAModules(modules, data)
    isa2:::isa.status("DONE", "out")
    modules
}
