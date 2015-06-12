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
  return(cameraRes)
}
voomCamera <- function(edgeObj, gscs) {
  obj.voom <- voom(dgeList(edgeObj))
  ctnames<- contrastNames(edgeObj)
  design <- designMatrix(edgeObj)
  ct <- contrastMatrix(edgeObj)
  geneSymbols <- fData(edgeObj)$GeneSymbol

  erTables <- lapply(gscs, function(gsc) {
    tt <- voomCameraGsc(obj.voom,
                        geneSymbols,
                        gsc, design=design, contrasts=ct)
    names(tt) <- ctnames
    return(tt)
  })
  names(erTables) <- names(gscs)
 
  edgeGse <-   as(edgeObj,"EdgeGSE")
  edgeGse@geneSetsList <- gscs
  edgeGse@method <- "voom+camera"
  edgeGse@enrichTables <- erTables
  return(edgeGse)
}

fullEnrichTable <- function(edgeGse) {
  tbls <- edgeGse@enrichTables
  tbl.genesets <- lapply(tbls, function(x) {
    res <- data.frame(GeneSet=unlist(lapply(x, rownames)),
                      Contrast=rep(names(x), sapply(x, nrow)),
                      do.call(rbind, x))
    rownames(res) <- NULL
    return(res)
  })
  res.base <- do.call(rbind, tbl.genesets)
  gscNames <- rep(names(edgeGse@geneSetsList), sapply(tbl.genesets, nrow))
  res <- data.frame(GeneSetList=gscNames,
                    res.base, row.names=NULL)

  return(res)
}
