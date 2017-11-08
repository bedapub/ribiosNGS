## TODO: replace data.frame with Objects

## gage method
myGage <- function(logFC, gsc) {
    genes <- gsGenes(gsc)
    cate <- gsCategory(gsc)
    gdf <- data.frame(geneset=names(genes), category=cate)
    gage.res <- gage::gage(logFC, gsets=genes, ref=NULL, samp=NULL)
    greater <- as.data.frame(gage.res[[1]][,c("stat.mean", "p.val", "q.val", "set.size")])
    less <- as.data.frame(gage.res[[2]][,c("p.val", "q.val")])
    greater$geneset <- rownames(greater)
    less$geneset <- rownames(less)
    greater <- merge(greater, gdf, by="geneset")
    less <- merge(less, gdf, by="geneset")
    res.raw <- merge(greater, less, by=c("geneset","category"),
                     suffix=c(".greater", ".less"))
    direction <- with(res.raw, ifelse(p.val.less<p.val.greater, "Down", "Up"))
    pVal.pmin <- with(res.raw, ifelse(p.val.less<p.val.greater, p.val.less, p.val.greater))
    pVal <- pVal.pmin * 2; pVal[pVal>1] <- 1

    ## TODO: contributing genes

    res <- data.frame(Category=res.raw$category,
                      GeneSet=res.raw$geneset,
                      NGenes=res.raw$set.size,
                      Direction=direction,
                      PValue=pVal,
                      FDR=rep(NA,length(pVal)))
    res <- subset(res, NGenes>=1 & !is.na(PValue) & !is.nan(PValue))
    res$FDR <- p.adjust(res$PValue, "fdr")
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
    edgeGse@enrichTables <- erTable
    return(edgeGse)
}

##----------------------------------------##
## camera
##----------------------------------------##
## voomCamera is outdated: use camera.EdgeResult instead
voomCamera <- function(edgeObj, gscs) {
  ## TODO: deprecate this after the script is fixed
  ## .Deprecated(new="camera.EdgeResult")
  obj.voom <- voom(edgeObj)
  ctnames<- contrastNames(edgeObj)
  design <- designMatrix(edgeObj)
  ct <- contrastMatrix(edgeObj)
  geneSymbols <- as.character(fData(edgeObj)$GeneSymbol)

  categories <- gsCategory(gscs)
  erTables <- tapply(gscs, categories, function(gsc) {
                         tt <- gscCamera(obj.voom,
                                         geneSymbols,
                                         gsc=gsc, design=design, contrasts=ct)
                         return(tt)
                     })

  erTable <- do.call(rbind, erTables)
  erTable$Category <- rep(names(erTables), sapply(erTables, nrow))
  erTable <- putColsFirst(erTable, "Category")
  rownames(erTable) <- NULL


  edgeGse <-   as(edgeObj,"EdgeGSE")
  edgeGse@geneSets <- gscs
  edgeGse@method <- "voom+camera"
  edgeGse@enrichTables <- erTable
  return(edgeGse)
}

#' Calculate mid-p quantile residuals
#' 
#' @param y An DGEList object
#' @param design Design matrix
#' @param contrast Contrast vector
#' 
#' The function is a carbon copy of edgeR:::.zscoreDGE, which is unfortunately not exported
#' 
#' @examples 
#' dgeMatrix <- matrix(rpois(1200, 10), nrow=200)
#' dgeList <- DGEList(dgeMatrix)
#' dgeList <- estimateCommonDisp(dgeList)
#' dgeDesign <- model.matrix(~gl(2,3))
#' dgeZscore <- zscoreDGE(dgeList, dgeDesign, contrast=c(0,1))
#' head(dgeZscore)
zscoreDGE <- function(y, design=NULL, contrast=ncol(design)) {
  allzero <- rowSums(y$counts > 1e-08) == 0
  if (any(allzero)) 
    warning(sum(allzero), "rows with all zero counts")
  dispersion <- getDispersion(y)
  if (is.null(dispersion)) 
    stop("Dispersion estimate not found. Please estimate the dispersion(s) before you proceed.")
  if (is.null(design)) {
    if (nlevels(y$samples$group) < 2) 
      stop("design not supplied and samples all belong to the same group")
    design <- model.matrix(~y$samples$group)
    rownames(design) <- colnames(y)
  }
  nbeta <- ncol(design)
  if (nbeta < 2) 
    stop("design matrix must have at least two columns")
  if (is.character(contrast)) {
    if (length(contrast) > 1) 
      stop("contrast should specify only one column of design")
    contrast <- which(contrast == colnames(design))
    if (!length(contrast)) 
      stop("contrast doesn't match any column of design")
  }
  if (length(contrast) == 1) {
    design0 <- design[, -contrast, drop = FALSE]
  }
  else {
    design <- contrastAsCoef(design, contrast = contrast, 
                             first = FALSE)$design
    design0 <- design[, -nbeta, drop = FALSE]
  }
  fit.null <- glmFit(y, design0, prior.count = 0)
  y <- zscoreNBinom(y$counts, mu = fit.null$fitted.values, 
                    size = 1/dispersion)
  y
}

#' Get human gene symbols for gene-set enrichment analysis
setGeneric("humanGeneSymbols", function(object) standardGeneric("humanGeneSymbols"))
fDataHumanGeneSymbol <- function(object) {
  gs <- fData(object)$GeneSymbol
  if(!is.character(gs) && !is.null(gs)) {
    gs <- as.character(gs)
  }
  return(gs)
}
setMethod("humanGeneSymbols", "DGEList", function(object) fDataHumanGeneSymbol(object))
setMethod("humanGeneSymbols", "EdgeObject", function(object) fDataHumanGeneSymbol(object))

#' Perform camera using DGEList
#' 
#' @param dgeList A DGEList object, with GeneSymbol available
#' @param index List of integer indices of genesets, names are namese of gene sets
#' @param design Design matrix
#' @param contrasts Contrast matrix
dgeListCamera <- function(dgeList, index, design, contrasts) {
  geneSymbols <- humanGeneSymbols(dgeList)
  if(is.null(geneSymbols))
    stop("EdgeResult must have 'GeneSymbol' in its fData to perform camera!")
  cameraRes <- mclapply(1:ncol(contrasts),
                      function(x) {
                        zscores <- zscoreDGE(y=dgeList,
                                             design=design,
                                             contrast=contrasts[,x])
                        tbl.priorCor <- camera(zscores,
                                               index=index,
                                               design=design,
                                               contrast=contrasts[,x],
                                               weights=NULL, ## TODO: may be passed in the future
                                               use.ranks = FALSE, ## TODO may be passed in the future
                                               allow.neg.cor=FALSE,
                                               inter.gene.cor=0.01,
                                               trend.var=FALSE,
                                               sort=FALSE)
                        tbl.estCor <- ribiosGSEA::biosCamera(zscores, 
                                                 index=index, 
                                                 design = design, contrast = contrasts[,x],
                                                 weights = NULL, ## TODO: may be passed in the future
                                                 use.ranks = FALSE, ## TODO may be passed in the future
                                                 geneLabels = geneSymbols, 
                                                 allow.neg.cor = FALSE, 
                                                 trend.var = FALSE, 
                                                 sort = FALSE)
                        if(length(index)==1) {
                          tbl.priorCor$FDR <- tbl.priorCor$PValue
                          tbl.estCor$FDR <- tbl.estCor$PValue
                        }
                        tbl <- cbind(tbl.estCor,
                                     PValue.cor0.01=tbl.priorCor$PValue,
                                     FDR.cor0.01=tbl.priorCor$FDR,
                                     Score.cor0.01=pScore(tbl.priorCor$PValue, tbl.priorCor$Direction=="Up"))
                        tbl$GeneSet <- rownames(tbl)
                        rownames(tbl) <- NULL
                        tbl <- tbl[,c("GeneSet", "NGenes","Direction",
                                      "Correlation", "PValue", "FDR", "Score",
                                      "PValue.cor0.01", "FDR.cor0.01", "Score.cor0.01", 
                                      "ContributingGenes")]
                        tbl <- sortByCol(tbl, "PValue",decreasing=FALSE)
                        return(tbl)
                      })
  
  cRes <- do.call(rbind, cameraRes)
  
  if(is.null(colnames(contrasts)))
    colnames(contrasts) <- sprintf("Contrast%d", 1:ncol(contrasts))
  
  bg <- data.frame(Contrast=rep(colnames(contrasts), sapply(cameraRes, nrow)))
  res <- cbind(bg, cRes)
  rownames(res) <- NULL
  res <- subset(res, NGenes>=1 & !is.na(PValue) & !is.nan(PValue))
  return(res)
}

#' Run CAMERA method using EdgeResult
#' 
#' @param y A EdgeResult object
#' @param gscs Gene set collections, for example read by \code{\link[ribiosGSEA]{readGmt}}
#' 
#' Note that the EdgeResult object must have a column 'GeneSymbol' in its \code{fData}.
camera.EdgeResult <- function(y, gscs) {
  ctnames<- contrastNames(y)
  design <- designMatrix(y)
  ct <- contrastMatrix(y)
  geneSymbols <- humanGeneSymbols(y)
  if(is.null(geneSymbols))
    stop("EdgeResult must have 'GeneSymbol' in its fData to perform camera!")

  categories <- ribiosGSEA::gsCategory(gscs)
  gsIndex <- ribiosGSEA::matchGenes(gscs, geneSymbols, na.rm=TRUE)
  names(gsIndex) <- make.unique(names(gsIndex))
  
  erTables <- tapply(seq(along=gsIndex), categories, function(i) {
    currInd <- gsIndex[i]
    tt <- dgeListCamera(y@dgeList,
                        index=currInd,
                        design=design, contrasts=ct)
    return(tt)
  })
  
  erTable <- do.call(rbind, erTables)
  erTable$Category <- rep(names(erTables), sapply(erTables, nrow))
  erTable <- putColsFirst(erTable, "Category")
  rownames(erTable) <- NULL
  
  
  edgeGse <-   as(y,"EdgeGSE")
  edgeGse@geneSets <- gscs
  edgeGse@method <- "camera.DGEList"
  edgeGse@enrichTables <- erTable
  return(edgeGse)
}

fullEnrichTable <- function(edgeGse) {
  return(edgeGse@enrichTables)
}
