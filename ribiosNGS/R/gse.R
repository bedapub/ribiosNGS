## TODO: replace data.frame with Objects

## gage method
myGage <- function(logFC, gsc) {
    genes <- gsGenes(gsc)
    gage.res <- gage(logFC, gsets=genes, ref=NULL, samp=NULL)
    greater <- as.data.frame(gage.res[[1]][,c("stat.mean", "p.val", "q.val", "set.size")])
    less <- as.data.frame(gage.res[[2]][,c("p.val", "q.val")])
    greater$geneset <- rownames(greater)
    less$geneset <- rownames(less)
    res.raw <- merge(greater, less, by="geneset", suffix=c(".greater", ".less"))
    direction <- with(res.raw, ifelse(p.val.less<p.val.greater, "Down", "Up"))
    pVal.pmin <- with(res.raw, ifelse(p.val.less<p.val.greater, p.val.less, p.val.greater))
    pVal <- pVal.pmin * 2; pVal[pVal>1] <- 1
    fdr <- p.adjust(pVal, "fdr")
    res <- data.frame(Category=gsCategory(gsc),
                      GeneSet=gsName(gsc),
                      NGenes=res.raw$set.size,
                      Direction=direction,
                      PValue=pVal,
                      FDR=fdr, row.names=res.raw$geneset)
    return(res)
}


logFCgage <- function(edgeResult, gscs) {
    geneSymbols <- fData(edgeResult)$GeneSymbol
    logFCs <- lapply(dgeTables(edgeResult), function(x) {
                         res <- x$logFC
                         names(res) <- x$GeneSymbol
                         return(res)
                     })
    erTables <- lapply(logFCs, function(x) myGage(x, gscs))
    
    erTable <- do.call(rbind, erTables)
    erTable$Contrast <- rep(names(logFCs),sapply(erTables, nrow))
    erTable <- putColsFirst(erTable, c("Category", "Contrast", "GeneSet"))
    rownames(erTable) <- NULL
    
    edgeGse <- as(edgeResult, "EdgeGSE")
    edgeGse@geneSets <- gscs
    edgeGse@method <- "gage"
    edgeGse@enrichTables <- erTables
    return(edgeGse)
}




voomCameraGsc <- function(voom, geneSymbols, gsc, design, contrasts) {
  genes <- gsGenes(gsc)
  genes.inds <- lapply(genes, function(x) {
    ind <- match(x, geneSymbols)
    return(ind[!is.na(ind)])
  })

  
  cameraRes <- lapply(1:ncol(contrasts),
                      function(x) camera(voom,
                                         design=design,
                                         index=genes.inds,
                                         contrast=contrasts[,x]))
  cRes <- do.call(rbind, cameraRes)
  bg <- data.frame(Contrast=rep(colnames(contrasts), sapply(cameraRes, nrow)),
                   GeneSet=rep(gsNames(gsc), ncol(contrasts)))
  res <- cbind(bg, cRes)
  rownames(res) <- NULL
  return(res)
}
voomCamera <- function(edgeObj, gscs) {
  obj.voom <- voom(dgeList(edgeObj))
  ctnames<- contrastNames(edgeObj)
  design <- designMatrix(edgeObj)
  ct <- contrastMatrix(edgeObj)
  geneSymbols <- fData(edgeObj)$GeneSymbol

  categories <- gsCategory(gscs)
  erTables <- tapply(gscs, categories, function(gsc) {
                         tt <- voomCameraGsc(obj.voom,
                                             geneSymbols,
                                             gsc=gsc, design=design, contrasts=ct)
                         return(tt)
                     })
  erTable <- do.call(rbind, erTables)
  erTable$Category <- categories
  erTable <- putColsFirst(erTable, "Category")
  rownames(erTable) <- NULL
  
  edgeGse <-   as(edgeObj,"EdgeGSE")
  edgeGse@geneSets <- gscs
  edgeGse@method <- "voom+camera"
  edgeGse@enrichTables <- erTable
  return(edgeGse)
}

fullEnrichTable <- function(edgeGse) {
  return(edgeGse@enrichTables)
}
